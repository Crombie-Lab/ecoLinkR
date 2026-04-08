#' Extract climate values at collection site locations
#'
#' Extracts climate data from raster files at specified geographic points (collection sites)
#' with optional buffer distance for neighborhood analysis. Creates a tidy table with
#' unique identifiers for joining back to collection data.
#'
#' The function accepts one or more raster files (GeoTIFF, etc.) and extracts values at
#' point locations. If a buffer distance is specified, calculates summary statistics
#' (mean, min, max) within a circular buffer around each point. Results are returned
#' in tidy format with a join key based on collection label.
#'
#' @param raster_paths Character vector. Path(s) to climate raster file(s).
#'   Can be a single raster or multiple rasters (one per variable).
#' @param collection_df Data frame with collection site locations.
#' @param lat_col Character. Name of latitude column in `collection_df`
#'   (default: `"collection_latitude"` or `"GPSLatitude"`).
#' @param lon_col Character. Name of longitude column in `collection_df`
#'   (default: `"collection_longitude"` or `"GPSLongitude"`).
#' @param label_col Character. Name of column with site labels/IDs for joining
#'   (default: `"c_label"`).
#' @param buffer_distance Numeric. Optional buffer distance in meters for neighborhood
#'   analysis. If `NULL` (default), extracts only the value at the exact point.
#' @param buffer_fun Character. Summary function to apply within buffer:
#'   `"mean"` (default), `"median"`, `"min"`, `"max"`, or `"sd"`.
#' @param var_names Character vector. Optional names for raster variables.
#'   If `NULL`, uses basenames of raster files.
#' @param crs Character. Coordinate reference system as EPSG code or proj string.
#'   Default: `"EPSG:4326"` (WGS84). Must match the CRS of input rasters.
#' @param out_csv Character. Optional path to write extracted values to CSV.
#'   If `NULL`, results are not written to disk.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item `c_label`: Collection label from input data (join key)
#'     \item Climate variable columns (one per raster file)
#'     \item `buffer_distance`: Distance used (if buffer applied)
#'     \item Any additional columns from `collection_df` (c_lat, c_lon, etc.)
#'   }
#'
#' @examples
#' \dontrun{
#' # Extract from a single raster
#' clim_vals <- extract_climate_values(
#'   raster_paths = "data/processed/prism_mean_tmean.tif",
#'   collection_df = collection_data,
#'   lat_col = "GPSLatitude",
#'   lon_col = "GPSLongitude",
#'   label_col = "c_label"
#' )
#'
#' # Extract from multiple rasters with buffer
#' clim_vals <- extract_climate_values(
#'   raster_paths = c(
#'     "data/processed/prism_tmean.tif",
#'     "data/processed/prism_ppt.tif"
#'   ),
#'   collection_df = collection_data,
#'   buffer_distance = 10000,  # 10 km buffer
#'   var_names = c("mean_temp_c", "mean_precip_mm"),
#'   out_csv = "data/processed/extracted_climate.csv"
#' )
#' }
#'
#' @export
extract_climate_values <- function(
    raster_paths,
    collection_df,
    lat_col = NULL,
    lon_col = NULL,
    label_col = "c_label",
    buffer_distance = NULL,
    buffer_fun = "mean",
    var_names = NULL,
    crs = "EPSG:4326",
    out_csv = NULL
) {

  # ---- validation ----
  if (!is.data.frame(collection_df)) {
    stop("collection_df must be a data frame")
  }

  if (length(raster_paths) == 0) {
    stop("At least one raster path must be provided")
  }

  # Check all rasters exist
  missing_rasters <- raster_paths[!file.exists(raster_paths)]
  if (length(missing_rasters) > 0) {
    stop("Raster file(s) not found:\n  - ",
         paste(missing_rasters, collapse = "\n  - "))
  }

  # ---- auto-detect lat/lon columns ----
  if (is.null(lat_col)) {
    lat_candidates <- c("collection_latitude", "GPSLatitude", "lat", "latitude")
    lat_col <- intersect(lat_candidates, names(collection_df))[1]
    if (is.na(lat_col)) {
      stop("Could not auto-detect latitude column. Specify 'lat_col' explicitly.\n",
           "Available columns: ", paste(names(collection_df), collapse = ", "))
    }
  }

  if (is.null(lon_col)) {
    lon_candidates <- c("collection_longitude", "GPSLongitude", "lon", "longitude")
    lon_col <- intersect(lon_candidates, names(collection_df))[1]
    if (is.na(lon_col)) {
      stop("Could not auto-detect longitude column. Specify 'lon_col' explicitly.\n",
           "Available columns: ", paste(names(collection_df), collapse = ", "))
    }
  }

  if (!label_col %in% names(collection_df)) {
    stop("label_col '", label_col, "' not found in collection_df.\n",
         "Available columns: ", paste(names(collection_df), collapse = ", "))
  }

  if (!lat_col %in% names(collection_df) || !lon_col %in% names(collection_df)) {
    stop("Latitude/longitude columns not found in collection_df")
  }

  # ---- set up variable names ----
  if (is.null(var_names)) {
    var_names <- tools::file_path_sans_ext(basename(raster_paths))
  } else if (length(var_names) != length(raster_paths)) {
    stop("Length of var_names must match length of raster_paths")
  }

  # ---- load rasters ----
  rasters <- lapply(raster_paths, terra::rast)

  # Check/reproject rasters if needed
  for (i in seq_along(rasters)) {
    r_crs <- terra::crs(rasters[[i]])
    if (is.na(r_crs) || r_crs == "") {
      warning("Raster ", i, " has no CRS defined. Assuming ", crs)
      terra::crs(rasters[[i]]) <- crs
    } else if (!identical(r_crs, crs)) {
      message("Reprojecting raster ", i, " to ", crs)
      rasters[[i]] <- terra::project(rasters[[i]], crs)
    }
  }

  # ---- prepare collection points ----
  coll_subset <- collection_df %>%
    dplyr::select(dplyr::all_of(c(label_col, lat_col, lon_col))) %>%
    dplyr::filter(!is.na(.data[[lat_col]]), !is.na(.data[[lon_col]])) %>%
    dplyr::distinct()

  if (nrow(coll_subset) == 0) {
    stop("No valid lat/lon coordinates found in collection_df")
  }

  # Create spatial points
  pts <- terra::vect(
    coll_subset,
    geom = c(lon_col, lat_col),
    crs = crs
  )

  # ---- extract values ----
  extracted_values <- list()

  for (i in seq_along(rasters)) {
    r <- rasters[[i]]
    var_name <- var_names[i]

    if (is.null(buffer_distance)) {
      # Point extraction
      vals <- terra::extract(r, pts)
      extracted_values[[i]] <- vals[[2]]  # First column is ID
    } else {
      # Buffer extraction with summary function
      buf_pts <- terra::buffer(pts, buffer_distance)
      vals_buf <- terra::extract(r, buf_pts)

      # We need to calculate the summary for each buffer
      # terra::extract returns a list/data frame with ID and extracted values from polygon
      # For a raster clipped to buffer, we compute the summary
      sum_fun <- switch(buffer_fun,
                       mean = mean,
                       median = median,
                       min = min,
                       max = max,
                       sd = sd,
                       stop("Unknown buffer_fun: ", buffer_fun))

      # Extract and summarize
      extracted_values[[i]] <- sapply(
        split(vals_buf[[2]], vals_buf[[1]]),
        function(x) sum_fun(x, na.rm = TRUE)
      )
    }
  }

  # ---- build result tibble ----
  result <- data.frame(
    c_label = coll_subset[[label_col]],
    c_latitude = coll_subset[[lat_col]],
    c_longitude = coll_subset[[lon_col]]
  )

  # Add extracted values
  for (i in seq_along(rasters)) {
    result[[var_names[i]]] <- extracted_values[[i]]
  }

  # Add buffer info if applicable
  if (!is.null(buffer_distance)) {
    result$buffer_distance <- buffer_distance
    result$buffer_fun <- buffer_fun
  }

  result <- tibble::as_tibble(result)

  # ---- write to CSV if requested ----
  if (!is.null(out_csv)) {
    out_dir <- dirname(out_csv)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    readr::write_csv(result, out_csv, na = "NA")
    message("Extracted climate values written to: ", out_csv)
  }

  result
}
