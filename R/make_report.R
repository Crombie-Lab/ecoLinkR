#' Create a collections R Markdown report
#'
#' Copies the package's collections skeleton to a new .Rmd file.
#'
#' @param path Where to write the new .Rmd file.
#' @param open Open the file in RStudio if possible.
#'
#' @export
make_report <- function(path = "wnc_report.Rmd", open = interactive()) {

  if (file.exists(path)) {
    stop("File already exists: ", path, call. = FALSE)
  }

  skel <- system.file(
    "rmarkdown", "templates", "wnc_report", "skeleton", "skeleton.Rmd",
    package = utils::packageName()
  )

  if (skel == "") {
    stop("Template skeleton not found.", call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file.copy(skel, path)

  if (open &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(path)
  }

  invisible(normalizePath(path))
}
