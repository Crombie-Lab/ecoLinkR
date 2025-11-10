#' Join collection/isolation data with GPS EXIF metadata
#'
#' Reads a processed collection/isolation dataset and an EXIF GPS dataset,
#' automatically detects and normalizes their join columns, performs a safe
#' left join, and summarizes the match results.
#'
#' Handles mismatched file paths or extensions automatically (e.g.,
#' "/Users/.../C-0001.png" vs "C-0001").
#'
#' @param coliso_csv Character. Path to the processed collection/isolation CSV file.
#' @param gps_csv Character. Path to the processed GPS EXIF metadata CSV file.
#' @param out_csv Character. Optional output CSV file path. If `NULL`
#'   (default), the result is returned but not written to disk.
#' @param join_key Optional manual join mapping (e.g. `c("raw_col_img_name" = "FileName")`).
#' @param keep_unmatched Logical. If `FALSE`, drops collection rows without GPS
#'   matches (default: `TRUE` keeps all).
#'
#' @return A tibble containing the joined dataset.
#' @importFrom stats setNames
#' @export
join_with_gps <- function(coliso_csv,
                          gps_csv,
                          out_csv = NULL,
                          join_key = NULL,
                          keep_unmatched = TRUE) {

  # ---- verify inputs ----
  if (!file.exists(coliso_csv)) stop("File not found: ", coliso_csv)
  if (!file.exists(gps_csv)) stop("File not found: ", gps_csv)

  # ---- read CSVs ----
  df1 <- readr::read_csv(coliso_csv, show_col_types = FALSE)
  df2 <- readr::read_csv(gps_csv, show_col_types = FALSE)

  # ---- normalize filename-like columns ----
  normalize_name <- function(x) tools::file_path_sans_ext(basename(trimws(x)))

  for (nm in c("FileName", "SourceFile")) {
    if (nm %in% names(df2)) df2[[nm]] <- normalize_name(df2[[nm]])
  }
  for (nm in c("raw_col_img_name", "raw_iso_img_name", "c_label")) {
    if (nm %in% names(df1)) df1[[nm]] <- normalize_name(df1[[nm]])
  }

  names1 <- names(df1)
  names2 <- names(df2)

  # ---- detect join columns ----
  if (!is.null(join_key)) {
    by_cols <- join_key
    message("Joining by user-defined key: ",
            paste(names(join_key), "=", unname(join_key)))
  } else {
    candidates1 <- c("c_label", "raw_col_img_name", "raw_iso_img_name",
                     "img_name", "filename", "FileName")
    candidates2 <- c("c_label", "raw_col_img_name", "raw_iso_img_name",
                     "img_name", "filename", "FileName", "SourceFile")

    match1 <- intersect(candidates1, names1)
    match2 <- intersect(candidates2, names2)

    if (length(match1) > 0 && length(match2) > 0) {
      by_cols <- setNames(match2[1], match1[1])
      message("Auto-detected join columns: ",
              match1[1], " (coliso) ", match2[1], " (gps)")
    } else {
      common_cols <- intersect(names1, names2)
      if (length(common_cols) == 0L) {
        warning("No shared or similar join columns found, returning original dataset unchanged.")
        return(df1)
      }
      by_cols <- common_cols
      message("Using shared column(s): ", paste(by_cols, collapse = ", "))
    }
  }

  # ---- perform join ----
  joined_df <- dplyr::left_join(df1, df2, by = by_cols)

  # ---- compute match summary ----
  n_left <- nrow(df1)
  n_right <- nrow(df2)
  join_col_left <- names(by_cols)[1]
  join_col_right <- unname(by_cols)[1]

  matched_keys <- intersect(df1[[join_col_left]], df2[[join_col_right]])
  n_matched <- length(matched_keys)
  pct <- 100 * n_matched / max(1, n_left)

  message(sprintf("Match summary: %d collection rows, %d GPS rows, %d matched (%.1f%% coverage)",
                  n_left, n_right, n_matched, pct))

  # ---- handle unmatched rows ----
  if (!keep_unmatched) {
    joined_df <- dplyr::filter(joined_df, .data[[join_col_left]] %in% matched_keys)
    message("Dropped unmatched collection rows (keep_unmatched = FALSE).")
  }

  # ---- check if GPS columns are filled ----
  gps_cols <- intersect(names(joined_df),
                        c("GPSLongitude", "GPSLatitude", "GPSAltitude"))
  if (length(gps_cols) > 0) {
    missing_gps <- sum(is.na(joined_df[[gps_cols[1]]]))
    if (missing_gps > 0)
      message(sprintf("%d records have missing GPS metadata.", missing_gps))
  }

  # ---- write to CSV ----
  if (!is.null(out_csv)) {
    out_dir <- dirname(out_csv)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    readr::write_csv(joined_df, out_csv, na = "")
    message("Joined data written to: ", out_csv)
  }

  joined_df
}

