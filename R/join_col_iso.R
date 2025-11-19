#' Join collection and isolation data with photo filenames
#'
#' Combines collection and isolation tables from Google Sheets, adds matching
#' collection and isolation photo filenames (based on \code{c_label}),
#' and optionally writes the joined table to CSV.
#'
#' @param c_url Character. Google Sheets URL for the collection data.
#' @param i_url Character. Google Sheets URL for the isolation data.
#' @param c_photo_dir Character. Directory containing collection photos.
#' @param i_photo_dir Character. Directory containing isolation photos.
#' @param out_dir Character. Directory where the joined CSV will be saved.
#' @param out_filename Character. Optional filename for the output CSV
#'   (default: \code{"joinedcoliso.csv"}).
#' @param write_csv Logical. If \code{TRUE} (default), write CSV to disk.
#'
#' @return A tibble containing joined data with photo filename columns.
#' @export
join_col_iso <- function(
    c_url,
    i_url,
    c_photo_dir,
    i_photo_dir,
    out_dir,
    out_filename = "joinedcoliso.csv",
    write_csv = TRUE
) {
  # ---- validate directories ----
  if (!dir.exists(c_photo_dir)) {
    warning("Collection photo directory not found: ", c_photo_dir)
  }
  if (!dir.exists(i_photo_dir)) {
    warning("Isolation photo directory not found: ", i_photo_dir)
  }
  if (!dir.exists(out_dir)) {
    message("Creating output directory: ", out_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # ---- read sheets ----
  col <- gsheet::gsheet2tbl(c_url)
  iso <- gsheet::gsheet2tbl(i_url)

  # ---- basic filtering ----
  if ("project" %in% names(col)) {
    col <- dplyr::filter(col, !is.na(.data$project))
  }
  if ("project_id" %in% names(iso)) {
    iso <- dplyr::filter(iso, !is.na(.data$project_id))
  }

  # ---- join tables ----
  join <- dplyr::full_join(col, iso, by = intersect(names(col), names(iso)))

  # ---- photo lists ----
  col_files <- if (dir.exists(c_photo_dir)) list.files(c_photo_dir) else character(0)
  iso_files <- if (dir.exists(i_photo_dir)) list.files(i_photo_dir) else character(0)

  col_photos <- tibble::tibble(raw_col_img_name = col_files) |>
    dplyr::mutate(c_label = sub("\\.[^.]*$", "", .data$raw_col_img_name))
  iso_photos <- tibble::tibble(raw_iso_img_name = iso_files) |>
    dplyr::mutate(c_label = sub("\\.[^.]*$", "", .data$raw_iso_img_name))

  # ensure c_label exists before joining
  if (!"c_label" %in% names(join)) join$c_label <- NA_character_

  # ---- join photo names ----
  join_p <- dplyr::left_join(join, col_photos, by = "c_label")
  if ("landscape" %in% names(join_p)) {
    join_p <- dplyr::relocate(join_p, "raw_col_img_name", .before = "landscape")
  }

  join_p2 <- dplyr::left_join(join_p, iso_photos, by = "c_label")
  if ("proliferation_48" %in% names(join_p2)) {
    join_p2 <- dplyr::relocate(join_p2, "raw_iso_img_name", .before = "proliferation_48")
  }

  # ---- output ----
  if (isTRUE(write_csv)) {
    out_path <- file.path(out_dir, out_filename)
    readr::write_csv(join_p2, out_path, na = "")
    message("Joined data written to: ", out_path)
    invisible(join_p2)
  } else {
    join_p2
  }
}
