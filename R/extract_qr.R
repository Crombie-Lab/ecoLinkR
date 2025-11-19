#' Extract QR code text and coordinates from images
#'
#' Scans PNG (or other) images in a directory for QR codes using \pkg{opencv},
#' and for each image with a detected QR code, writes a CSV file containing
#' the QR polygon coordinates and decoded text.
#'
#' Filenames are normalized to a base identifier by stripping directory
#' and extension (e.g., "C-0003.png" -> "C-0003"), then converted to
#' lowercase and with dashes removed (e.g., "c0003") for the CSV basename.
#'
#' @param img_dir Character. Directory containing input images (e.g.
#'   "data/processed/photos/isolation").
#' @param output_dir Character. Directory where per-image CSV files will be
#'   written. The directory will be created if it does not exist.
#' @param pattern Character. Regular expression for image file extensions.
#'   Defaults to "\\\\.png$".
#' @param recursive Logical. Whether to search subdirectories recursively
#'   (default: \code{FALSE}).
#'
#' @return Invisibly returns a data frame summarizing which files had QR codes
#'   detected, the decoded text, and the CSV paths (if written).
#'
#' @examples
#' \dontrun{
#' extract_qr(
#'   img_dir = "data/processed/photos/isolation",
#'   output_dir = "data/processed/extracted_qr_text/isolation"
#' )
#' }
#'
#' @importFrom tools file_path_sans_ext
#' @export
extract_qr <- function(img_dir,
                           output_dir,
                           pattern = "\\.png$",
                           recursive = FALSE) {

  # ---- validate directories ----
  if (!dir.exists(img_dir)) {
    stop("Image directory not found: ", img_dir)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created output directory: ", output_dir)
  }

  # ---- list images ----
  png_files <- list.files(
    img_dir,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )

  if (length(png_files) == 0L) {
    warning("No image files found in ", img_dir, " matching pattern: ", pattern)
    return(invisible(
      data.frame(
        file = character(0),
        qr_text = character(0),
        detected = logical(0),
        csv_path = character(0),
        stringsAsFactors = FALSE
      )
    ))
  }

  # ---- loop over images ----
  results <- vector("list", length(png_files))

  for (i in seq_along(png_files)) {
    img_path <- png_files[i]
    file_base <- tools::file_path_sans_ext(basename(img_path))  # e.g. "C-0003"
    csv_base <- tolower(gsub("-", "", file_base))               # e.g. "c0003"

    # Read and detect QR
    qr_text <- NA_character_
    csv_path <- NA_character_
    detected <- FALSE

    qr_points_df <- NULL

    # Wrap in tryCatch so one bad file does not abort the whole run
    qr_result <- tryCatch(
      {
        img <- opencv::ocv_read(img_path)
        opencv::ocv_qr_detect(img)
      },
      error = function(e) {
        message("Error processing ", file_base, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(qr_result)) {
      qr_text_chr <- as.character(qr_result)
      qr_points <- attr(qr_result, "points")

      if (!is.null(qr_points)) {
        qr_points_df <- as.data.frame(qr_points)
      }

      if (!is.null(qr_points_df) &&
          nchar(qr_text_chr) > 0 &&
          nrow(qr_points_df) > 0) {

        detected <- TRUE
        qr_text <- qr_text_chr

        # Append decoded text to all rows
        qr_points_df$text <- qr_text_chr

        csv_path <- file.path(output_dir, paste0(csv_base, "_qrdata.csv"))
        utils::write.csv(qr_points_df, csv_path, row.names = FALSE)

        message("QR code found in ", file_base, "; data written to: ", csv_path)
      } else {
        message("No QR code detected in ", file_base)
      }
    } else {
      message("Skipping file due to read/detect error: ", file_base)
    }

    results[[i]] <- data.frame(
      file = basename(img_path),
      qr_text = ifelse(is.na(qr_text), "", qr_text),
      detected = detected,
      csv_path = ifelse(is.na(csv_path), "", csv_path),
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, results)

  invisible(summary_df)
}
