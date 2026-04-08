#' Download or locate climate raster data
#'
#' Downloads climate data from PRISM or returns paths to existing raster files.
#' This function provides a convenient interface to obtain climate data (temperature,
#' precipitation, etc.) for a specified geographic area and time period.
#'
#' Currently supports:
#' - **PRISM**: Mean daily temperature and precipitation (requires \pkg{prism} package)
#' - **Existing files**: Returns paths to manually downloaded rasters in a directory
#'
#' If `use_prism = TRUE`, the \pkg{prism} package must be installed. For offline
#' use or when PRISM is unavailable, provide a directory with existing GeoTIFF files.
#'
#' @param variable Character. Climate variable(s) to download: `"tmean"` (mean temperature),
#'   `"ppt"` (precipitation), or a vector of both `c("tmean", "ppt")`.
#' @param start_date Character in format `"YYYY-MM-DD"`. Start date for climate period.
#' @param end_date Character in format `"YYYY-MM-DD"`. End date for climate period.
#' @param use_prism Logical. If `TRUE`, attempt to download from PRISM. If `FALSE`,
#'   look for existing rasters in `raster_dir` (default: `TRUE`).
#' @param raster_dir Character. Directory containing existing raster files or where
#'   downloaded files will be saved. If `NULL`, uses a temporary directory.
#' @param out_list Logical. If `TRUE`, return a named list of raster paths.
#'   If `FALSE`, return a data frame. (default: `TRUE`)
#'
#' @return If `out_list = TRUE`: A named list of paths to raster files
#'   (e.g., `list(tmean = path1, ppt = path2)`).\cr
#'   If `out_list = FALSE`: A data frame with columns `variable`, `path`, `date_range`.
#'
#' @examples
#' \dontrun{
#' # Download PRISM data and save to a directory
#' clim_data <- download_climate_data(
#'   variable = c("tmean", "ppt"),
#'   start_date = "2024-11-01",
#'   end_date = "2025-02-01",
#'   raster_dir = "data/climate",
#'   use_prism = TRUE
#' )
#'
#' # Use existing rasters from a directory
#' clim_data <- download_climate_data(
#'   variable = c("tmean", "ppt"),
#'   raster_dir = "data/processed",
#'   use_prism = FALSE
#' )
#' }
#'
#' @export
download_climate_data <- function(
    variable = c("tmean", "ppt"),
    start_date = NULL,
    end_date = NULL,
    use_prism = FALSE,
    raster_dir = NULL,
    out_list = TRUE
) {

  # ---- validation ----
  if (length(variable) == 0) {
    stop("At least one variable must be specified.")
  }

  variable <- match.arg(variable, c("tmean", "ppt"), several.ok = TRUE)

  # ---- check for prism package if use_prism = TRUE ----
  if (use_prism) {
    if (!requireNamespace("prism", quietly = TRUE)) {
      stop("The 'prism' package is required to download PRISM data.\n",
           "Install it with: install.packages('prism')")
    }
    
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("The 'terra' package is required for processing rasters.\n",
           "Install it with: install.packages('terra')")
    }

    if (is.null(start_date) || is.null(end_date)) {
      stop("start_date and end_date are required when use_prism = TRUE")
    }

    # Convert dates
    start <- as.Date(start_date)
    end <- as.Date(end_date)

    if (is.na(start) || is.na(end)) {
      stop("start_date and end_date must be valid dates in 'YYYY-MM-DD' format")
    }

    # Setup download directory
    if (is.null(raster_dir)) {
      raster_dir <- tempdir()
    }
    
    if (!dir.exists(raster_dir)) {
      dir.create(raster_dir, recursive = TRUE)
    }

    # Set prism download directory
    prism::prism_set_dl_dir(raster_dir)

    # Download and process each variable
    results <- list()
    
    for (var in variable) {
      message("Downloading PRISM ", var, " data from ", start_date, " to ", end_date, "...")
      
      prism_var <- switch(var,
                         tmean = "tmean",
                         ppt = "ppt",
                         stop("Unknown variable: ", var))

      # Download daily data
      tryCatch({
        prism::get_prism_dailys(
          type = prism_var,
          minDate = start,
          maxDate = end,
          keepZip = FALSE
        )

        message("Download complete. Searching for raster files...")

        # After download, search the prism data directory for the downloaded files
        # Get the prism data directory
        prism_data_dir <- prism::prism_get_dl_dir()
        
        # Search recursively for raster files matching this variable
        all_files <- list.files(
          prism_data_dir,
          pattern = paste0(prism_var, ".*\\.(bil|tif|tiff)$"),
          full.names = TRUE,
          recursive = TRUE,
          ignore.case = TRUE
        )
        
        if (length(all_files) == 0) {
          warning("No PRISM files found for variable: ", var, 
                  " in directory: ", prism_data_dir)
          next
        }

        message("Found ", length(all_files), " raster files for ", var)
        
        # Stack rasters
        tryCatch({
          r_stack <- terra::rast(all_files)
          
          # Calculate mean across time period
          r_mean <- terra::mean(r_stack, na.rm = TRUE)

          # Save as GeoTIFF
          out_file <- file.path(raster_dir, paste0("prism_mean_", var, "_", 
                                                     gsub("-", "", start_date), "_",
                                                     gsub("-", "", end_date), ".tif"))
          terra::writeRaster(r_mean, out_file, overwrite = TRUE)
          
          results[[var]] <- out_file
          message("Saved: ", out_file)
        }, error = function(e) {
          warning("Error stacking or saving rasters for ", var, ": ", e$message)
        })

      }, error = function(e) {
        warning("Failed to download/process ", var, ": ", e$message)
      })
    }

    if (length(results) == 0) {
      stop("No PRISM data successfully downloaded.")
    }

    if (out_list) {
      return(results)
    } else {
      df <- data.frame(
        variable = names(results),
        path = unlist(results, use.names = FALSE),
        date_range = paste0(start_date, " to ", end_date),
        stringsAsFactors = FALSE
      )
      return(df)
    }

  } else {
    # ---- use existing rasters ----
    if (is.null(raster_dir)) {
      stop("raster_dir must be provided when use_prism = FALSE")
    }

    if (!dir.exists(raster_dir)) {
      stop("raster_dir not found: ", raster_dir)
    }

    return(list_climate_rasters(raster_dir = raster_dir, variable = variable, out_list = out_list))
  }
}


# ---- Helper function to list and return climate rasters ----
list_climate_rasters <- function(raster_dir, variable, out_list = TRUE) {
  # Find raster files (GeoTIFF, BIL, and other common formats)
  # Search recursively to find files in subdirectories (e.g., PRISM date folders)
  raster_files <- list.files(
    raster_dir,
    pattern = "\\.(tif|tiff|TIF|TIFF|bil|BIL|img|IMG|grd|GRD)$",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(raster_files) == 0) {
    warning("No raster files found in: ", raster_dir)
    if (out_list) return(list()) else return(data.frame())
  }

  # Try to match raster files to requested variables
  results <- list()
  for (var in variable) {
    pattern <- switch(var,
                     tmean = "tmean|temp|temperature",
                     ppt = "ppt|precip|precipitation",
                     var)

    matching <- raster_files[grepl(pattern, raster_files, ignore.case = TRUE)]

    if (length(matching) == 0) {
      warning("No raster file found matching variable: ", var)
    } else {
      # Take the first match (or modify logic if multiple time periods needed)
      results[[var]] <- matching[1]
    }
  }

  if (out_list) {
    return(results)
  } else {
    # Convert to data frame
    df <- data.frame(
      variable = names(results),
      path = unlist(results, use.names = FALSE),
      stringsAsFactors = FALSE
    )
    return(df)
  }
}
