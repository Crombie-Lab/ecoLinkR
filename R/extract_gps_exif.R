#' Extract GPS EXIF metadata from image files
#'
#' Reads image files (e.g., PNG, JPG, HEIC) from a directory, extracts EXIF
#' metadata using \pkg{exifr}, selects GPS-related fields, and optionally writes
#' the results to a CSV file.
#'
#' @param img_dir Character. Directory containing image files.
#' @param extensions Character vector of file extensions to include (default:
#'   c("png", "jpg", "jpeg", "heic")).
#' @param out_csv Character. Optional output CSV file path. If `NULL`
#'   (default), the results are not written to disk.
#' @param recursive Logical. Whether to search subdirectories recursively
#'   (default: `FALSE`).
#'
#' @return A tibble containing selected EXIF metadata columns:
#'   \code{SourceFile}, \code{DateTimeOriginal}, \code{GPSLongitude},
#'   \code{GPSLatitude}, and \code{GPSAltitude}.
#'
#' @examples
#' \dontrun{
#' extract_gps_exif(
#'   img_dir = "data/processed/photos/collection",
#'   extensions = c("png", "jpg", "heic"),
#'   out_csv = "data/processed/gpsdata202411fltech.csv"
#' )
#' }
#'
#' @export
extract_gps_exif <- function(
    img_dir,
    extensions = c("png", "jpg", "jpeg", "heic"),
    out_csv = NULL,
    recursive = FALSE
) {
  # ---- check directory ----
  if (!dir.exists(img_dir)) {
    stop("Image directory not found: ", img_dir)
  }

  # ---- list images ----
  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$", collapse = "")
  image_files <- list.files(
    path = img_dir,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )

  if (length(image_files) == 0L) {
    warning("No image files found in ", img_dir, " matching pattern: ", pattern)
    return(tibble::tibble())
  }

  # ---- extract EXIF ----
  exif_data <- exifr::read_exif(image_files)

  # ---- select relevant columns ----
  gps_cols <- c("SourceFile", "FileName", "DateTimeOriginal",
                "GPSLongitude", "GPSLatitude", "GPSAltitude")

  gps_data <- dplyr::select(exif_data, dplyr::any_of(gps_cols))

  # ---- write to CSV if requested ----
  if (!is.null(out_csv)) {
    out_dir <- dirname(out_csv)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    readr::write_csv(gps_data, out_csv, na = "")
    message("EXIF data written to: ", out_csv)
  }

  gps_data
}
