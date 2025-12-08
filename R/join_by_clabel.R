#' Stack multiple collection tables by c_label
#'
#' This helper stacks any number of data frames on top of each other
#' (row-binds them), while enforcing the presence of a `c_label` column.
#' It is useful for combining project-specific tables (e.g. 202411_FLTECH,
#' 202412_TCS, 202502_FLTECH) into a single long table of collections.
#'
#' Unlike a join, this does *not* merge rows with the same `c_label`
#' across tables; it simply concatenates all rows.
#'
#' @param ... Either:
#'   * multiple data frames, each containing a `c_label` column, **or**
#'   * a single list of such data frames.
#'
#' @return A data frame created by \code{dplyr::bind_rows()}.
#'   All inputs are row-bound; columns are the union of all input columns.
#'
#' @examples
#' \dontrun{
#' col_202411 <- readr::read_csv("joined_with_gps_202411fltech.csv")
#' col_202412 <- readr::read_csv("joined_with_gps_202412TCS.csv")
#' col_202502 <- readr::read_csv("joined_with_gps_202502fltech.csv")
#'
#' all_projects <- bind_by_clabel(col_202411, col_202412, col_202502)
#' }
#'
#' @export
join_by_clabel <- function(...) {
  dots <- list(...)

  # Allow either bind_by_clabel(df1, df2, ...)
  # or bind_by_clabel(list(df1, df2, ...))
  if (length(dots) == 1L && !is.data.frame(dots[[1L]])) {
    dots <- dots[[1L]]
  }

  if (!length(dots)) {
    stop("No input data frames supplied to `bind_by_clabel()`.", call. = FALSE)
  }

  # Basic checks + ensure c_label is character in each input
  for (i in seq_along(dots)) {
    if (!is.data.frame(dots[[i]])) {
      stop("Element ", i, " supplied to `bind_by_clabel()` is not a data frame.",
           call. = FALSE)
    }
    if (!"c_label" %in% names(dots[[i]])) {
      stop("Element ", i, " does not contain a `c_label` column.",
           call. = FALSE)
    }
    dots[[i]][["c_label"]] <- as.character(dots[[i]][["c_label"]])
  }

  dplyr::bind_rows(dots)
}
