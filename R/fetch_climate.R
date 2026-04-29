#' Download and process climate raster tiles
#'
#' Downloads daily climate data from PRISM, stacks across a date range, crops to a
#' region of interest, calculates temporal summaries (mean for temperature, mean
#' for precipitation), and writes processed GeoTIF tiles ready for mapping and analysis.
#'
#' The function:
#' 1. Downloads daily PRISM raster data for specified dates
#' 2. Stacks all daily rasters for the period
#' 3. Crops to user-specified latitude/longitude bounds
#' 4. Calculates temporal mean (across dates) for each variable
#' 5. Writes high-quality GeoTIFF output files
#'
#' Requires the \pkg{prism} and \pkg{terra} packages.
#'
#' @param variable Character. Climate variable(s): `"tmean"` (mean daily temperature),
#'   `"ppt"` (precipitation), or `c("tmean", "ppt")` for both.
#' @param start_date Character in format `"YYYY-MM-DD"`. Start date for climate period.
#' @param end_date Character in format `"YYYY-MM-DD"`. End date for climate period.
#' @param lat_min Numeric. Minimum latitude for cropping (required).
#' @param lat_max Numeric. Maximum latitude for cropping (required).
#' @param lon_min Numeric. Minimum longitude for cropping (required).
#' @param lon_max Numeric. Maximum longitude for cropping (required).
#' @param out_dir Character. Directory where processed GeoTIFF tiles will be saved.
#'   If `NULL`, uses `tempdir()`. Default: `NULL`.
#' @param resolution Character. PRISM resolution: `"800m"` (daily 800m) or `"4km"` 
#'   (coarser, more complete). Default: `"800m"`.
#' @param overwrite Logical. If `TRUE`, overwrite existing output files. Default: `FALSE`.
#' @param out_list Logical. If `TRUE` (default), return a named list of output paths.
#'   If `FALSE`, return a data frame with columns `variable`, `path`, `date_range`.
#' @param lulc_tiles Character vector of ESA WorldCover tile URLs to download, or `NULL`
#'   (default) to skip land cover. When provided, tiles are downloaded (or read from
#'   cache), cropped to the supplied bounding box, mosaicked, and aggregated to
#'   ~300 m resolution. The processed raster is saved as a GeoTIFF and its path is
#'   returned as `result$lulc`. Tiles are available from
#'   `https://esa-worldcover.s3.amazonaws.com/v200/2021/map/`.
#' @param lulc_cache_dir Character. Directory used to cache raw ESA WorldCover tile
#'   downloads. Defaults to a `lulc_tiles/` sub-folder inside `out_dir`. Set this to
#'   a permanent location to avoid re-downloading tiles between R sessions.
#' @param lulc_agg_factor Integer. Aggregation factor applied to the mosaicked land
#'   cover raster before saving (default: `30`, ≈ 300 m from 10 m source tiles).
#'
#' @return If `out_list = TRUE`: A named list with elements `tmean` and/or `ppt`
#'   containing paths to the output GeoTIFF files.\cr
#'   If `out_list = FALSE`: A data frame with columns `variable`, `path`, `date_range`.
#'
#' @details
#' Output GeoTIFF files are named with the pattern:
#' `prism_mean_{variable}_{start_date}_{end_date}.tif`
#' and contain raster data cropped to your specified region with values representing
#' the mean across all days in the date range.
#'
#' @examples
#' \dontrun{
#' # Download PRISM data, crop to Miami region, average over 3 months
#' clim_tiles <- fetch_climate(
#'   variable = c("tmean", "ppt"),
#'   start_date = "2024-11-01",
#'   end_date = "2025-02-01",
#'   lat_min = 28.00,
#'   lat_max = 28.40,
#'   lon_min = -80.63,
#'   lon_max = -80.60,
#'   out_dir = "data/climate/processed",
#'   resolution = "800m"
#' )
#'
#' # Use the output in mapping
#' r_temp <- terra::rast(clim_tiles$tmean)
#' r_precip <- terra::rast(clim_tiles$ppt)
#' }
#'
#' @export
fetch_climate <- function(
    variable = c("tmean", "ppt"),
    start_date = NULL,
    end_date = NULL,
    lat_min = NULL,
    lat_max = NULL,
    lon_min = NULL,
    lon_max = NULL,
    out_dir = NULL,
    resolution = "800m",
    overwrite = FALSE,
    out_list = TRUE,
    lulc_tiles = NULL,
    lulc_cache_dir = NULL,
    lulc_agg_factor = 30L
) {

  # ---- Validation ----
  if (is.null(start_date) || is.null(end_date)) {
    stop("start_date and end_date are required (format: 'YYYY-MM-DD')")
  }

  if (is.null(lat_min) || is.null(lat_max) || is.null(lon_min) || is.null(lon_max)) {
    stop("All of lat_min, lat_max, lon_min, lon_max are required for cropping")
  }

  if (!(is.numeric(lat_min) && is.numeric(lat_max) && is.numeric(lon_min) && is.numeric(lon_max))) {
    stop("lat_min, lat_max, lon_min, lon_max must all be numeric")
  }

  if (lat_min >= lat_max) {
    stop("lat_min must be less than lat_max")
  }

  if (lon_min >= lon_max) {
    stop("lon_min must be less than lon_max")
  }

  # Validate dates
  start <- tryCatch(as.Date(start_date), error = function(e) NA)
  end <- tryCatch(as.Date(end_date), error = function(e) NA)

  if (is.na(start) || is.na(end)) {
    stop("start_date and end_date must be valid dates in 'YYYY-MM-DD' format")
  }

  if (start > end) {
    stop("start_date must be before or equal to end_date")
  }

  variable <- match.arg(variable, c("tmean", "ppt"), several.ok = TRUE)

  if (!requireNamespace("prism", quietly = TRUE)) {
    stop("The 'prism' package is required. Install with: install.packages('prism')")
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The 'terra' package is required. Install with: install.packages('terra')")
  }

  # ---- Setup ----
  if (is.null(out_dir)) {
    out_dir <- tempdir()
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Temporary directory for raw PRISM downloads
  dl_dir <- file.path(out_dir, "raw_prism_download")
  if (!dir.exists(dl_dir)) {
    dir.create(dl_dir, recursive = TRUE, showWarnings = FALSE)
  }

  prism::prism_set_dl_dir(dl_dir)

  # Increase download timeout for large PRISM requests (PRISM uses download.file internally)
  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  # Bounding box for cropping
  roi <- terra::ext(lon_min, lon_max, lat_min, lat_max)

  results <- list()

  # ---- Process each variable ----
  for (var in variable) {
    message("Processing PRISM ", var, " from ", start_date, " to ", end_date, "...")

    prism_var <- switch(var,
      tmean = "tmean",
      ppt = "ppt",
      stop("Unknown variable: ", var)
    )

    tryCatch({
      # Download daily data (retry up to 3 times with backoff)
      message("  Downloading ", prism_var, " daily data...")

      max_retries <- 3
      download_result <- FALSE
      for (attempt in seq_len(max_retries)) {
        if (attempt > 1) {
          wait_secs <- 15 * (attempt - 1)
          message("  Retry attempt ", attempt, " of ", max_retries,
                  " for ", var, " (waiting ", wait_secs, "s)...")
          Sys.sleep(wait_secs)
        }
        download_result <- tryCatch({
          prism::get_prism_dailys(
            type = prism_var,
            minDate = start,
            maxDate = end,
            keepZip = FALSE
          )
          TRUE
        }, error = function(e) {
          message("    Download error (attempt ", attempt, "): ", e$message)
          FALSE
        })
        if (download_result) break
      }

      if (!download_result) {
        warning("Failed to download ", var, " data after ", max_retries, " attempts")
        next
      }

      # Get downloaded files directly from the download directory
      message("  Locating downloaded raster files...")
      
      # Look for .bil files (raw PRISM) in the download directory
      # They may be in nested subdirectories like prism_tmean_us_25m_20241101/
      all_bil_files <- list.files(
        dl_dir,
        pattern = "\\.bil$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
      )
      
      if (length(all_bil_files) == 0) {
        warning("No PRISM .bil files found in download directory: ", dl_dir)
        next
      }
      
      # Filter for the current variable
      # Match files that contain tmean or ppt in their filename
      if (prism_var == "tmean") {
        pattern <- "tmean"
      } else if (prism_var == "ppt") {
        pattern <- "ppt"
      } else {
        pattern <- prism_var
      }
      
      prism_files <- all_bil_files[grepl(pattern, basename(all_bil_files), ignore.case = TRUE)]
      
      if (length(prism_files) == 0) {
        warning("No files matching variable '", var, "' found. ",
                "Looking for files with '", pattern, "' in name.")
        message("Available files: ", paste(head(basename(all_bil_files), 5), collapse = ", "))
        next
      }
      
      message("  Found ", length(prism_files), " raster files for ", var)

      # Load and crop all rasters individually
      message("  Loading and cropping ", length(prism_files), " daily rasters...")
      cropped_rasters <- list()
      
      for (i in seq_along(prism_files)) {
        r <- tryCatch({
          r_raw <- terra::rast(prism_files[i])
          terra::crop(r_raw, roi)
        }, error = function(e) {
          warning("Failed to load/crop raster ", i, ": ", e$message)
          return(NULL)
        })
        
        if (!is.null(r)) {
          cropped_rasters[[i]] <- r
        }
      }
      
      if (length(cropped_rasters) == 0) {
        warning("No rasters were successfully loaded/cropped for ", var)
        next
      }
      
      message("  Resampling all rasters to common grid...")
      
      # Use the first raster as the reference grid
      reference <- cropped_rasters[[1]]
      resampled_rasters <- list(reference)
      
      if (length(cropped_rasters) > 1) {
        # Resample remaining rasters to match the first one
        for (i in 2:length(cropped_rasters)) {
          r_resampled <- tryCatch({
            terra::resample(cropped_rasters[[i]], reference, method = "bilinear")
          }, error = function(e) {
            warning("Failed to resample raster ", i, ": ", e$message)
            return(NULL)
          })
          
          if (!is.null(r_resampled)) {
            resampled_rasters[[i]] <- r_resampled
          }
        }
      }
      
      if (length(resampled_rasters) < 1) {
        warning("No rasters available for ", var)
        next
      }
      
      message("  Stacking ", length(resampled_rasters), " aligned rasters...")
      r_stack <- tryCatch({
        terra::rast(resampled_rasters)
      }, error = function(e) {
        warning("Failed to stack rasters for ", var, ": ", e$message)
        return(NULL)
      })
      
      if (is.null(r_stack)) {
        next
      }

      # Calculate temporal mean
      message("  Calculating temporal mean...")
      r_mean <- tryCatch({
        terra::mean(r_stack, na.rm = TRUE)
      }, error = function(e) {
        warning("Failed to calculate mean for ", var, ": ", e$message)
        return(NULL)
      })
      
      if (is.null(r_mean)) {
        next
      }

      # Build output filename
      out_file <- file.path(
        out_dir,
        paste0(
          "prism_mean_", var, "_",
          gsub("-", "", start_date), "_",
          gsub("-", "", end_date),
          ".tif"
        )
      )

      # Write GeoTIFF
      message("  Writing output raster...")
      write_result <- tryCatch({
        terra::writeRaster(r_mean, out_file, overwrite = overwrite)
        TRUE
      }, error = function(e) {
        warning("Failed to write raster for ", var, ": ", e$message)
        FALSE
      })
      
      if (!write_result) {
        next
      }
      
      message("  Saved: ", out_file)
      results[[var]] <- out_file

    }, error = function(e) {
      warning("Failed to process ", var, ": ", e$message)
    })
  }

  if (length(results) == 0) {
    stop("No climate data successfully processed")
  }

  # ---- Optional: download & process ESA WorldCover land cover tiles ----
  if (!is.null(lulc_tiles) && length(lulc_tiles) > 0) {
    message("Processing ESA WorldCover land cover tiles...")

    tile_cache <- if (!is.null(lulc_cache_dir)) {
      lulc_cache_dir
    } else {
      file.path(out_dir, "lulc_tiles")
    }
    if (!dir.exists(tile_cache)) dir.create(tile_cache, recursive = TRUE)

    tile_files <- character(0)
    for (i in seq_along(lulc_tiles)) {
      fname <- file.path(tile_cache, basename(lulc_tiles[i]))
      if (!file.exists(fname) || isTRUE(overwrite)) {
        message(sprintf("  LULC tile %d/%d: downloading...", i, length(lulc_tiles)))
        tryCatch(
          utils::download.file(lulc_tiles[i], fname, mode = "wb", quiet = TRUE),
          error = function(e) message("  WARNING - Failed: ", e$message)
        )
      } else {
        message(sprintf("  LULC tile %d/%d: cached", i, length(lulc_tiles)))
      }
      if (file.exists(fname)) tile_files <- c(tile_files, fname)
    }

    if (length(tile_files) == 0) {
      warning("No LULC tiles available; land cover skipped.")
    } else {
      lulc_ext <- terra::ext(lon_min, lon_max, lat_min, lat_max)
      tile_rasts <- lapply(tile_files, function(f) {
        r <- terra::rast(f)
        tryCatch(terra::crop(r, lulc_ext), error = function(e) {
          warning("Could not crop LULC tile: ", basename(f)); NULL
        })
      })
      tile_rasts <- Filter(Negate(is.null), tile_rasts)

      if (length(tile_rasts) > 0) {
        lulc_mosaic <- if (length(tile_rasts) == 1) tile_rasts[[1]] else
          do.call(terra::mosaic, c(tile_rasts, list(fun = "first")))
        lulc_agg  <- terra::aggregate(lulc_mosaic, fact = lulc_agg_factor, fun = "modal")
        lulc_agg  <- terra::project(lulc_agg, "EPSG:4326")

        lulc_out <- file.path(
          out_dir,
          paste0("lulc_esa_worldcover_",
                 gsub("[^0-9]", "", lat_min), "N_",
                 gsub("[^0-9]", "", abs(lon_min)), "W.tif")
        )
        terra::writeRaster(lulc_agg, lulc_out, overwrite = TRUE)
        message("  LULC raster saved: ", lulc_out)
        results[["lulc"]] <- lulc_out
      } else {
        warning("No LULC tiles overlapped the bounding box; land cover skipped.")
      }
    }
  }

  # ---- Return ----
  if (out_list) {
    return(results)
  }

  data.frame(
    variable = names(results),
    path = unlist(results, use.names = FALSE),
    date_range = paste0(start_date, " to ", end_date),
    stringsAsFactors = FALSE
  )
}

#' @export
#' @rdname fetch_climate
download_climate_data <- fetch_climate