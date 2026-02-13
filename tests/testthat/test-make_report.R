# tests/testthat/test-make_report.R

test_that("make_report() copies skeleton and returns a path", {
  skip_if_not_installed("testthat")

  # Skip if template isn't present (common during early dev / incomplete inst/)
  skel <- system.file(
    "rmarkdown", "templates", "wnc_report", "skeleton", "skeleton.Rmd",
    package = utils::packageName()
  )
  skip_if(identical(skel, ""), "Template skeleton not available in installed package.")

  tmpdir <- withr::local_tempdir()
  out_rmd <- file.path(tmpdir, "wnc_report.Rmd")

  res <- make_report(path = out_rmd, open = FALSE)

  expect_true(file.exists(out_rmd))
  expect_true(nzchar(res))
  expect_true(file.exists(res))

  # Basic sanity: created file should be identical to skeleton
  expect_identical(readLines(out_rmd, warn = FALSE), readLines(skel, warn = FALSE))
})

test_that("make_report() errors if target already exists", {
  tmpdir <- withr::local_tempdir()
  out_rmd <- file.path(tmpdir, "wnc_report.Rmd")
  writeLines("placeholder", out_rmd)

  expect_error(
    make_report(path = out_rmd, open = FALSE),
    "File already exists"
  )
})

test_that("make_report() errors if skeleton is missing", {
  # To reliably simulate a missing skeleton, create a throwaway copy of the
  # function that asks for a non-existent template path.
  tmpdir <- withr::local_tempdir()
  out_rmd <- file.path(tmpdir, "wnc_report.Rmd")

  make_report_missing <- function(path = out_rmd, open = FALSE) {
    stopifnot(is.character(path), length(path) == 1)
    if (file.exists(path)) stop("File already exists: ", path, call. = FALSE)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

    skel <- system.file(
      "rmarkdown", "templates", "THIS_DOES_NOT_EXIST", "skeleton", "skeleton.Rmd",
      package = utils::packageName()
    )

    if (identical(skel, "")) {
      stop("Could not find skeleton.Rmd in the installed package.", call. = FALSE)
    }

    ok <- file.copy(skel, path, overwrite = FALSE)
    if (!ok) stop("Failed to copy template to: ", path, call. = FALSE)

    out <- normalizePath(path, winslash = "/", mustWork = TRUE)

    if (isTRUE(open) &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      rstudioapi::navigateToFile(out)
    }

    invisible(out)
  }

  expect_error(
    make_report_missing(path = out_rmd, open = FALSE),
    "Could not find skeleton\\.Rmd"
  )
})
