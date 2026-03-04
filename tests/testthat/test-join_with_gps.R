# tests/testthat/test-join_with_gps.R

test_that("join_with_gps left-joins with robust filename normalization", {
  skip_if_not_installed("readr")
  skip_if_not_installed("dplyr")

  coliso <- data.frame(
    raw_col_img_name = c(
      "path/to/C-0001.JPG",
      "C_0002.jpeg",
      "c 0003.png",
      "no_match.jpg"
    ),
    other_col = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )

  exif <- data.frame(
    FileName = c(
      "c0001",          # matches C-0001.JPG
      "C-0002.JPG",     # matches C_0002.jpeg
      "C-0003.PNG"      # matches "c 0003.png"
    ),
    GPSLatitude  = c(28.1, 28.2, 28.3),
    GPSLongitude = c(-80.1, -80.2, -80.3),
    DateTimeOriginal = c("2025:11:25 10:00:00", "2025:11:25 10:01:00", "2025:11:25 10:02:00"),
    stringsAsFactors = FALSE
  )

  coliso_path <- tempfile(fileext = ".csv")
  exif_path   <- tempfile(fileext = ".csv")

  readr::write_csv(coliso, coliso_path)
  readr::write_csv(exif, exif_path)

  out <- join_with_gps(
    coliso_csv = coliso_path,
    exif_csv   = exif_path,
    verbose    = FALSE
  )

  # left join keeps all coliso rows
  expect_equal(nrow(out), nrow(coliso))

  # the standardized rename layer should produce these columns (no exif_ prefix)
  expect_true(all(c("GPSLatitude", "GPSLongitude", "DateTimeOriginal", "FileName") %in% names(out)))

  # matched rows get GPS; unmatched stays NA
  expect_false(is.na(out$GPSLatitude[1]))
  expect_false(is.na(out$GPSLatitude[2]))
  expect_false(is.na(out$GPSLatitude[3]))
  expect_true(is.na(out$GPSLatitude[4]))
})

test_that("join_with_gps can drop unmatched rows when keep_unmatched = FALSE", {
  skip_if_not_installed("readr")
  skip_if_not_installed("dplyr")

  coliso <- data.frame(
    raw_col_img_name = c("C-0001.JPG", "no_match.jpg"),
    stringsAsFactors = FALSE
  )

  exif <- data.frame(
    FileName = c("c0001"),
    GPSLatitude = 28.1,
    GPSLongitude = -80.1,
    stringsAsFactors = FALSE
  )

  coliso_path <- tempfile(fileext = ".csv")
  exif_path   <- tempfile(fileext = ".csv")
  readr::write_csv(coliso, coliso_path)
  readr::write_csv(exif, exif_path)

  out <- join_with_gps(
    coliso_csv = coliso_path,
    exif_csv   = exif_path,
    keep_unmatched = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(out), 1)
  expect_true("GPSLatitude" %in% names(out))
  expect_false(is.na(out$GPSLatitude[1]))
})

test_that("join_with_gps warns on duplicate EXIF join keys and keeps first", {
  skip_if_not_installed("readr")
  skip_if_not_installed("dplyr")

  coliso <- data.frame(
    raw_col_img_name = c("C-0001.JPG"),
    stringsAsFactors = FALSE
  )

  # Two EXIF rows that normalize to the same key (c0001)
  exif <- data.frame(
    FileName = c("C-0001.JPG", "c0001.png"),
    GPSLatitude  = c(28.1, 99.9),
    GPSLongitude = c(-80.1,  0.0),
    stringsAsFactors = FALSE
  )

  coliso_path <- tempfile(fileext = ".csv")
  exif_path   <- tempfile(fileext = ".csv")
  readr::write_csv(coliso, coliso_path)
  readr::write_csv(exif, exif_path)

  expect_warning(
    out <- join_with_gps(
      coliso_csv = coliso_path,
      exif_csv   = exif_path,
      verbose    = FALSE
    ),
    "Duplicate EXIF join keys detected"
  )

  # It should keep the first occurrence per key
  expect_equal(out$GPSLatitude[1], 28.1)
  expect_equal(out$GPSLongitude[1], -80.1)
})

test_that("join_with_gps errors when files are missing or join columns absent", {
  skip_if_not_installed("readr")
  skip_if_not_installed("dplyr")

  expect_error(
    join_with_gps(coliso_csv = "does_not_exist.csv", exif_csv = "nope.csv", verbose = FALSE),
    "File not found"
  )

  coliso <- data.frame(raw_col_img_name = "C-0001.JPG", stringsAsFactors = FALSE)
  exif   <- data.frame(FileName = "c0001", stringsAsFactors = FALSE)

  coliso_path <- tempfile(fileext = ".csv")
  exif_path   <- tempfile(fileext = ".csv")
  readr::write_csv(coliso, coliso_path)
  readr::write_csv(exif, exif_path)

  expect_error(
    join_with_gps(coliso_csv = coliso_path, exif_csv = exif_path, coliso_img_col = "bad_col", verbose = FALSE),
    "coliso_img_col not found"
  )

  expect_error(
    join_with_gps(coliso_csv = coliso_path, exif_csv = exif_path, exif_img_col = "bad_col", verbose = FALSE),
    "exif_img_col not found"
  )
})
