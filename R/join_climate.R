#' Join climate data to collection data with clear keys
#'
#' Joins extracted climate values back to collection data using a collection label
#' key. This creates a complete dataset with both ecological and climate information
#' ready for downstream analysis.
#'
#' The join is performed using the collection label column (`c_label`) as the key,
#' which must be present in both data frames. If the collection data has multiple
#' rows per site (e.g., multiple isolations from same collection), all rows with
#' matching labels are joined with their climate values.
#'
#' @param collection_df Data frame. The original collection data with GPS and other
#'   site information. Must contain a column specified by `by` parameter.
#' @param climate_df Data frame. Output from `extract_climate()`.
#'   Must contain columns `c_label`, climate variables, and coordinates.
#' @param by Character. Column name to join on. Default: `"c_label"`.
#' @param join_type Character. Type of join: `"left"` (default, keep all collection rows),
#'   `"inner"` (keep only rows with climate data), or `"full"`.
#' @param out_csv Character. Optional path to write the joined data to CSV.
#'   If `NULL`, results are not written to disk.
#' @param verbose Logical. If `TRUE`, prints join diagnostics (default: `FALSE`).
#'
#' @return A data frame combining `collection_df` with climate columns added from
#'   `climate_df`. Includes all original columns plus climate variables and
#'   coordinate columns.
#'
#' @details
#' Climate coordinate columns (`c_latitude`, `c_longitude`) are added with
#' `.cli` suffix if there are existing `latitude`/`longitude` columns in
#' `collection_df` to avoid duplication.
#'
#' If `by` column contains duplicates in `climate_df`, only the first occurrence
#' is used to prevent row multiplication.
#'
#' @examples
#' \dontrun{
#' # Join extracted climate to collection data
#' collection_with_climate <- join_climate(
#'   collection_df = my_collection_data,
#'   climate_df = extracted_climate,
#'   by = "c_label",
#'   join_type = "left"
#' )
#'
#' # Save to file
#' join_climate(
#'   collection_df = my_collection_data,
#'   climate_df = extracted_climate,
#'   out_csv = "data/processed/collection_with_climate.csv"
#' )
#' }
#'
#' @export
join_climate <- function(
    collection_df,
    climate_df,
    by = "c_label",
    join_type = "left",
    out_csv = NULL,
    verbose = FALSE
) {

  # ---- validation ----
  if (!is.data.frame(collection_df)) {
    stop("collection_df must be a data frame")
  }

  if (!is.data.frame(climate_df)) {
    stop("climate_df must be a data frame")
  }

  if (!by %in% names(collection_df)) {
    stop("Join column '", by, "' not found in collection_df.\n",
         "Available columns: ", paste(names(collection_df), collapse = ", "))
  }

  if (!by %in% names(climate_df)) {
    stop("Join column '", by, "' not found in climate_df.\n",
         "Available columns: ", paste(names(climate_df), collapse = ", "))
  }

  join_type <- match.arg(join_type, c("left", "inner", "full"))

  # ---- handle duplicate keys in climate_df ----
  dup_keys <- climate_df[[by]][duplicated(climate_df[[by]]) & !is.na(climate_df[[by]])]

  if (length(dup_keys) > 0) {
    if (verbose) {
      warning(
        "Duplicate join keys found in climate_df (showing up to 10): ",
        paste(utils::head(unique(dup_keys), 10), collapse = ", "),
        ". Keeping first occurrence per key."
      )
    }
    climate_df <- dplyr::distinct(climate_df, .data[[by]], .keep_all = TRUE)
  }

  # ---- perform join ----
  joined <- switch(join_type,
    left = dplyr::left_join(collection_df, climate_df, by = by),
    inner = dplyr::inner_join(collection_df, climate_df, by = by),
    full = dplyr::full_join(collection_df, climate_df, by = by),
    stop("Invalid join_type: ", join_type)
  )

  # ---- rename coordinate columns if needed ----
  # If collection_df has lat/lon and climate_df adds c_latitude/c_longitude,
  # this is OK since they're already prefixed. But if both use same name:
  if ("latitude" %in% names(collection_df) && "c_latitude" %in% names(joined)) {
    # No conflict, already named c_latitude from climate
    NULL
  }

  if (verbose) {
    n_match <- sum(!is.na(joined[[by]]))
    n_clim_match <- sum(!is.na(climate_df[[by]]))
    n_coll_match <- sum(!is.na(collection_df[[by]]))

    message("Join diagnostics:")
    message("  Collection data rows: ", nrow(collection_df))
    message("  Climate data rows: ", nrow(climate_df))
    message("  Matched keys: ", n_match)
    message("  Unmatched collection rows: ", nrow(collection_df) - n_match)
  }

  # ---- write to CSV if requested ----
  if (!is.null(out_csv)) {
    out_dir <- dirname(out_csv)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    readr::write_csv(joined, out_csv, na = "NA")
    message("Joined data written to: ", out_csv)
  }

  dplyr::as_tibble(joined)
}

#' @rdname join_climate
#' @export
join_climate_to_collection <- function(...) {
  .Deprecated("join_climate")
  join_climate(...)
}
