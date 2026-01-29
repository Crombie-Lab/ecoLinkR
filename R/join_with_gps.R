#' Join collection/isolation table with EXIF/GPS table
#'
#' Reads two CSVs, normalizes filename-like keys (to make joins robust),
#' left-joins GPS/EXIF metadata onto the collection/isolation table, and
#' optionally writes the result to disk.
#'
#' Key normalization is designed to match common Crombie-lab style IDs:
#' - strips paths + extensions
#' - lowercases
#' - removes dashes/underscores/spaces
#' - removes URL query strings (e.g., ?raw=1)
#'
#' @param coliso_csv Path to the collection/isolation CSV.
#' @param gps_csv Path to the EXIF/GPS CSV.
#' @param out_csv Optional path to write the joined CSV.
#' @param join_key Named character vector mapping left->right join columns
#'   (e.g. c("raw_col_img_name" = "FileName")).
#'   If NULL, attempts to auto-detect a reasonable pair.
#' @param keep_unmatched Logical; if FALSE, drops rows in coliso with no match.
#' @param normalize_keys Logical; if TRUE (default), creates normalized join
#'   helper columns and joins on those.
#' @param verbose Logical; print diagnostics and match summaries.
#'
#' @return A data frame of joined data.
#' @export
join_with_gps <- function(coliso_csv,
                          gps_csv,
                          out_csv = NULL,
                          join_key = NULL,
                          keep_unmatched = TRUE,
                          normalize_keys = TRUE,
                          verbose = TRUE) {

  # ---- verify inputs ----
  if (!file.exists(coliso_csv)) stop("File not found: ", coliso_csv)
  if (!file.exists(gps_csv)) stop("File not found: ", gps_csv)

  # ---- read CSVs ----
  df1 <- readr::read_csv(coliso_csv, show_col_types = FALSE)
  df2 <- readr::read_csv(gps_csv, show_col_types = FALSE)

  # ---- helper: robust filename normalization ----
  normalize_name <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- sub("\\?.*$", "", x)                 # drop URL query strings
    x <- basename(x)                          # drop directories
    x <- tools::file_path_sans_ext(x)         # drop extensions
    x <- tolower(x)                           # normalize case
    x <- gsub("[-_\\s]", "", x)               # remove -, _, spaces
    x
  }

  # ---- determine join columns ----
  names1 <- names(df1)
  names2 <- names(df2)

  if (!is.null(join_key)) {
    # expect named vector: left_name = right_name
    if (is.null(names(join_key)) || any(names(join_key) == "")) {
      stop("join_key must be a *named* character vector, e.g. c('raw_col_img_name'='FileName').")
    }
    left_key  <- names(join_key)[1]
    right_key <- unname(join_key)[1]

    if (!left_key %in% names1) stop("Left join column not found in coliso: ", left_key)
    if (!right_key %in% names2) stop("Right join column not found in gps: ", right_key)

    if (verbose) {
      message("Joining by user-defined key: ", left_key, " (coliso) = ", right_key, " (gps)")
    }
  } else {
    # auto-detect reasonable join columns
    candidates_left  <- c("raw_col_img_name", "raw_iso_img_name", "c_label", "img_name", "filename", "FileName")
    candidates_right <- c("FileName", "SourceFile", "raw_col_img_name", "raw_iso_img_name", "c_label", "img_name", "filename")

    left_matches  <- intersect(candidates_left, names1)
    right_matches <- intersect(candidates_right, names2)

    if (length(left_matches) == 0 || length(right_matches) == 0) {
      common_cols <- intersect(names1, names2)
      if (length(common_cols) == 0) {
        warning("No joinable columns found; returning original dataset unchanged.")
        return(df1)
      }
      left_key <- common_cols[1]
      right_key <- common_cols[1]
      if (verbose) message("Auto-detected shared join column: ", left_key)
    } else {
      left_key <- left_matches[1]
      # pick best matching right key preference order:
      # if left is a filename-ish column, prefer FileName/SourceFile
      if (left_key %in% c("raw_col_img_name", "raw_iso_img_name", "img_name", "filename", "FileName")) {
        right_key <- if ("FileName" %in% right_matches) "FileName" else right_matches[1]
        if ("SourceFile" %in% right_matches) right_key <- right_key # keep FileName priority; SourceFile used if no FileName
        if (!"FileName" %in% right_matches && "SourceFile" %in% right_matches) right_key <- "SourceFile"
      } else {
        right_key <- right_matches[1]
      }
      if (verbose) message("Auto-detected join columns: ", left_key, " (coliso) = ", right_key, " (gps)")
    }
  }

  # ---- normalize join keys in helper columns (recommended) ----
  if (normalize_keys) {
    df1 <- dplyr::mutate(df1, .join_key = normalize_name(.data[[left_key]]))
    df2 <- dplyr::mutate(df2, .join_key = normalize_name(.data[[right_key]]))

    # perform join on helper key
    joined_df <- dplyr::left_join(df1, df2, by = ".join_key", suffix = c(".coliso", ".gps"))
  } else {
    # legacy: join on raw columns directly
    joined_df <- dplyr::left_join(df1, df2, by = stats::setNames(right_key, left_key),
                                  suffix = c(".coliso", ".gps"))
  }

  # ---- match summary (row-level, not unique-key-level) ----
  if (verbose) {
    if (normalize_keys) {
      rows_matched <- sum(!is.na(joined_df$.join_key) & joined_df$.join_key %in% df2$.join_key, na.rm = TRUE)
      coverage <- 100 * rows_matched / max(1, nrow(df1))

      message(sprintf(
        "Match summary: %d coliso rows, %d gps rows, %d rows matched (%.1f%% coverage)",
        nrow(df1), nrow(df2), rows_matched, coverage
      ))

      # show a few unmatched examples for debugging
      if (rows_matched == 0) {
        message("No matches detected. Example normalized keys:")
        message("  coliso .join_key: ", paste(head(df1$.join_key, 10), collapse = ", "))
        message("  gps   .join_key: ", paste(head(df2$.join_key, 10), collapse = ", "))
      }
    }
  }

  # ---- drop unmatched if requested ----
  if (!keep_unmatched) {
    if (normalize_keys) {
      joined_df <- dplyr::filter(joined_df, .data$.join_key %in% df2$.join_key)
    } else {
      # if not normalized, "matched" means the left key exists on the right
      joined_df <- dplyr::filter(joined_df, .data[[left_key]] %in% df2[[right_key]])
    }
    if (verbose) message("Dropped unmatched coliso rows (keep_unmatched = FALSE).")
  }

  # ---- optional: quick GPS presence check (flexible column names) ----
  if (verbose) {
    gps_name_hits <- names(joined_df)[grepl("gps|latitude|longitude|altitude|lat$|lon$",
                                            names(joined_df), ignore.case = TRUE)]
    if (length(gps_name_hits) > 0) {
      # pick a likely lat column for NA counting
      lat_candidates <- gps_name_hits[grepl("lat", gps_name_hits, ignore.case = TRUE)]
      if (length(lat_candidates) > 0) {
        na_lat <- sum(is.na(joined_df[[lat_candidates[1]]]))
        message(sprintf("%d/%d records have missing '%s'.",
                        na_lat, nrow(joined_df), lat_candidates[1]))
      }
    } else {
      message("No obvious GPS columns detected in joined output (by name pattern).")
    }
  }

  # ---- write to CSV ----
  if (!is.null(out_csv)) {
    out_dir <- dirname(out_csv)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    readr::write_csv(joined_df, out_csv, na = "")
    if (verbose) message("Joined data written to: ", out_csv)
  }

  joined_df
}
