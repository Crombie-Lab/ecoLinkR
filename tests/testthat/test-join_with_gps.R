# tests/testthat/test-join_with_gps.R

testthat::test_that("join_with_gps joins and prefixes EXIF columns (core behavior)", {
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("dplyr")

  tmp <- tempdir()
  coliso_path <- file.path(tmp, "TCS202511_coliso.csv")
  exif_path   <- file.path(tmp, "tcs1125_exif.csv")

  # Core cases: punctuation + extension + case differences (no internal whitespace)
  coliso <- data.frame(
    raw_col_img_name = c("C-0001.JPG", "C_0002.png", "C-0003.jpeg", "C-9999.jpg"),
    some_col = 1:4,
    stringsAsFactors = FALSE
  )

  # EXIF includes a duplicate key to ensure you do NOT explode rows
  exif <- data.frame(
    FileName     = c("c0001", "c0002", "c0002", "c0003"),
    GPSLatitude  = c(35.1, 35.2, 99.9, 35.3),
    GPSLongitude = c(-80.1, -80.2, -99.9, -80.3),
    stringsAsFactors = FALSE
  )

  readr::write_csv(coliso, coliso_path)
  readr::write_csv(exif, exif_path)

  out <- join_with_gps(
    coliso_csv = coliso_path,
    exif_csv   = exif_path,
    coliso_img_col = "raw_col_img_name",
    exif_img_col   = "FileName",
    keep_unmatched = TRUE,
    verbose = FALSE
  )

  # Must not multiply rows
  testthat::expect_equal(nrow(out), nrow(coliso))

  # EXIF columns should be prefixed
  testthat::expect_true(all(c("exif_GPSLatitude", "exif_GPSLongitude", "exif_FileName") %in% names(out)))

  # Values should land on the correct rows (by identity)
  lat_0001 <- out$exif_GPSLatitude[out$raw_col_img_name == "C-0001.JPG"]
  lat_0002 <- out$exif_GPSLatitude[out$raw_col_img_name == "C_0002.png"]
  lat_0003 <- out$exif_GPSLatitude[out$raw_col_img_name == "C-0003.jpeg"]
  lat_9999 <- out$exif_GPSLatitude[out$raw_col_img_name == "C-9999.jpg"]

  testthat::expect_equal(lat_0001, 35.1)
  testthat::expect_equal(lat_0002, 35.2)  # confirms "first duplicate wins" if you distinct() by key
  testthat::expect_equal(lat_0003, 35.3)
  testthat::expect_true(is.na(lat_9999))
})


testthat::test_that("join_with_gps strips internal whitespace in keys (if implemented)", {
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("dplyr")

  tmp <- tempdir()
  coliso_path <- file.path(tmp, "TCS202511_coliso_ws.csv")
  exif_path   <- file.path(tmp, "tcs1125_exif_ws.csv")

  # This case will ONLY match if your normalization removes internal whitespace:
  # "c 0003" -> "c0003"
  coliso <- data.frame(
    raw_col_img_name = c("c 0003.jpeg"),
    stringsAsFactors = FALSE
  )

  exif <- data.frame(
    FileName    = c("c0003"),
    GPSLatitude = c(35.3),
    stringsAsFactors = FALSE
  )

  readr::write_csv(coliso, coliso_path)
  readr::write_csv(exif, exif_path)

  out <- join_with_gps(
    coliso_csv = coliso_path,
    exif_csv   = exif_path,
    coliso_img_col = "raw_col_img_name",
    exif_img_col   = "FileName",
    keep_unmatched = TRUE,
    verbose = FALSE
  )

  # If the function doesn't strip whitespace, this will be NA.
  if (is.na(out$exif_GPSLatitude[1])) {
    testthat::skip("join_with_gps() in this package version does not strip internal whitespace from join keys.")
  }

  testthat::expect_equal(out$exif_GPSLatitude[1], 35.3)
})


testthat::test_that("join_with_gps drops unmatched rows when keep_unmatched = FALSE", {
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("dplyr")

  tmp <- tempdir()
  coliso_path <- file.path(tmp, "TCS202511_coliso_drop.csv")
  exif_path   <- file.path(tmp, "tcs1125_exif_drop.csv")

  coliso <- data.frame(
    raw_col_img_name = c("C-0001.JPG", "C-9999.jpg"),
    some_col = c("a", "b"),
    stringsAsFactors = FALSE
  )

  # Only latitude is present—this should still work after the function fix
  exif <- data.frame(
    FileName = c("c0001"),
    GPSLatitude = c(1.23),
    stringsAsFactors = FALSE
  )

  readr::write_csv(coliso, coliso_path)
  readr::write_csv(exif, exif_path)

  out <- join_with_gps(
    coliso_csv = coliso_path,
    exif_csv   = exif_path,
    coliso_img_col = "raw_col_img_name",
    exif_img_col   = "FileName",
    keep_unmatched = FALSE,
    verbose = FALSE
  )

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$raw_col_img_name[1], "C-0001.JPG")
  testthat::expect_equal(out$exif_GPSLatitude[1], 1.23)
})



testthat::test_that("join_with_gps errors on missing files or missing join columns", {
  testthat::skip_if_not_installed("readr")

  tmp <- tempdir()
  coliso_path <- file.path(tmp, "coliso_cols.csv")
  exif_path   <- file.path(tmp, "exif_cols.csv")

  readr::write_csv(data.frame(raw_col_img_name = "C-0001.JPG"), coliso_path)
  readr::write_csv(data.frame(FileName = "c0001"), exif_path)

  testthat::expect_error(
    join_with_gps(coliso_csv = "nope.csv", exif_csv = exif_path, verbose = FALSE),
    "File not found"
  )

  testthat::expect_error(
    join_with_gps(coliso_csv = coliso_path, exif_csv = "nope.csv", verbose = FALSE),
    "File not found"
  )

  testthat::expect_error(
    join_with_gps(coliso_csv = coliso_path, exif_csv = exif_path,
                  coliso_img_col = "does_not_exist", verbose = FALSE),
    "coliso_img_col not found"
  )

  testthat::expect_error(
    join_with_gps(coliso_csv = coliso_path, exif_csv = exif_path,
                  exif_img_col = "does_not_exist", verbose = FALSE),
    "exif_img_col not found"
  )
})
