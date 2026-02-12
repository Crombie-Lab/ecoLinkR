#' Join collection/isolation data frame with GPS data frame
#'
#'Creates a new data table displaying the complete collection/isolation data
#'along with the GPS metadata extracted from the collection images.
#'
#' It performs a **left join** (keeps all coliso rows), using a robust
#' filename normalization so joins work even when one table has paths,
#' extensions, mixed case, or punctuation differences (e.g. `C-0001.JPG`
#' vs `c0001`).
#'
#' By default it joins:
#' - coliso: `raw_col_img_name`
#' - exif:  `FileName`
#'
#' EXIF columns are prefixed with `exif_` to avoid collisions and to make
#' provenance obvious in the final table.
#'
#' @param coliso_csv Path to the coliso CSV (e.g., `TCS202511_coliso.csv`)
#' @param exif_csv Path to the exif CSV (e.g., `tcs1125_exif.csv`)
#' @param out_csv Optional path to write the joined CSV. If NULL, nothing is written.
#' @param coliso_img_col Column in coliso to join on. Defaults to `raw_col_img_name`.
#' @param exif_img_col Column in exif to join on. Defaults to `FileName`.
#' @param keep_unmatched If FALSE, drops coliso rows with no EXIF match.
#' @param verbose If TRUE, prints match diagnostics.
#'
#' @return A data frame: coliso with EXIF/GPS columns appended.
#' @export
join_with_gps <- function(coliso_csv,
                          exif_csv = NULL,
                          gps_csv = NULL,
                          out_csv = NULL,
                          coliso_img_col = "raw_col_img_name",
                          exif_img_col   = "FileName",
                          keep_unmatched = TRUE,
                          verbose = TRUE) {

  # Backwards compatibility: allow gps_csv as an alias of exif_csv
  if (is.null(exif_csv) && !is.null(gps_csv)) exif_csv <- gps_csv
  if (is.null(exif_csv)) stop("You must supply exif_csv (or gps_csv).")

  # ---- checks ----
  if (!file.exists(coliso_csv)) stop("File not found: ", coliso_csv)
  if (!file.exists(exif_csv)) stop("File not found: ", exif_csv)

  df_coliso <- readr::read_csv(coliso_csv, show_col_types = FALSE)
  df_exif   <- readr::read_csv(exif_csv, show_col_types = FALSE)

  if (!coliso_img_col %in% names(df_coliso)) {
    stop("coliso_img_col not found in coliso: ", coliso_img_col)
  }
  if (!exif_img_col %in% names(df_exif)) {
    stop("exif_img_col not found in exif: ", exif_img_col)
  }

  # ---- robust key normalization ----
  normalize_name <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- sub("\\?.*$", "", x)                  # drop URL query strings
    x <- basename(x)                           # drop directories
    x <- tools::file_path_sans_ext(x)          # drop extensions
    x <- tolower(x)                            # normalize case
    x <- gsub("[-_\\s]", "", x)                # remove dashes/underscores/spaces
    x
  }

  df_coliso <- dplyr::mutate(df_coliso, .join_key = normalize_name(.data[[coliso_img_col]]))
  df_exif   <- dplyr::mutate(df_exif,   .join_key = normalize_name(.data[[exif_img_col]]))

  # ---- handle duplicate EXIF keys (prevents row multiplication on join) ----
  dup_keys <- df_exif$.join_key[duplicated(df_exif$.join_key) & !is.na(df_exif$.join_key)]
  if (length(dup_keys) > 0) {
    warning(
      "Duplicate EXIF join keys detected (showing up to 10): ",
      paste(utils::head(unique(dup_keys), 10), collapse = ", "),
      ". Keeping the first occurrence per key."
    )
    df_exif <- dplyr::distinct(df_exif, .join_key, .keep_all = TRUE)
  }

  # ---- rename EXIF columns with prefix to keep them from "disappearing" ----
  # Keep the original exif filename column too (as exif_FileName by default).
  exif_cols_to_rename <- setdiff(names(df_exif), ".join_key")
  df_exif_prefixed <- dplyr::rename_with(
    df_exif,
    ~ paste0("exif_", .x),
    .cols = dplyr::all_of(exif_cols_to_rename)
  )

  # ---- join ----
  joined <- dplyr::left_join(df_coliso, df_exif_prefixed, by = ".join_key")

  # ---- optional drop unmatched ----
  if (!keep_unmatched) {
    joined <- dplyr::filter(joined, .join_key %in% df_exif$.join_key)
    if (verbose) message("Dropped unmatched rows (keep_unmatched = FALSE).")
  }


  # ---- diagnostics ----
  if (verbose) {
    rows_matched <- sum(df_coliso$.join_key %in% df_exif$.join_key, na.rm = TRUE)
    coverage <- 100 * rows_matched / max(1, nrow(df_coliso))
    message(sprintf(
      "Match summary: %d coliso rows, %d exif rows, %d rows matched (%.1f%% coverage).",
      nrow(df_coliso), nrow(df_exif), rows_matched, coverage
    ))

    # show whether GPS is actually being populated
    if ("exif_GPSLatitude" %in% names(joined) && "exif_GPSLongitude" %in% names(joined)) {
      n_with_gps <- sum(!is.na(joined$exif_GPSLatitude) | !is.na(joined$exif_GPSLongitude), na.rm = TRUE)
      message(sprintf("Rows with any GPS (lat/lon not NA): %d/%d", n_with_gps, nrow(joined)))
    }
  }

  # ---- clean up ----
  joined <- dplyr::select(joined, -dplyr::any_of(".join_key"))

  # ---- write output ----
  if (!is.null(out_csv)) {
    out_dir <- dirname(out_csv)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    readr::write_csv(joined, out_csv, na = "")
    if (verbose) message("Wrote joined CSV: ", out_csv)
  }

  joined
}
