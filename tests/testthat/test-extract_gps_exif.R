test_that("extract_gps_exif handles mixed extensions and writes correctly", {
  # create temporary test directories
  tmp_dir <- file.path(tempdir(), "photos_test")
  dir.create(tmp_dir, showWarnings = FALSE)

  # create dummy image files for multiple extensions
  dummy_files <- c("img1.jpg", "img2.png", "img3.heic", "img4.jpeg")
  file.create(file.path(tmp_dir, dummy_files))

  # create a temporary output path
  tmp_csv <- file.path(tempdir(), "gps_test_output.csv")

  # ---- mock exifr::read_exif() to avoid reading real EXIF data ----
  testthat::local_mocked_bindings(
    read_exif = function(files) {
      # Return mock EXIF-like data
      tibble::tibble(
        SourceFile = files,
        FileName = basename(files),
        DateTimeOriginal = rep(Sys.time(), length(files)),
        GPSLongitude = runif(length(files), -90, 90),
        GPSLatitude = runif(length(files), -180, 180),
        GPSAltitude = runif(length(files), 0, 100)
      )
    },
    .package = "exifr"
  )

  # ---- run the function ----
  result <- extract_gps_exif(
    img_dir = tmp_dir,
    extensions = c("png", "jpg", "jpeg", "heic"),
    out_csv = tmp_csv
  )

  # ---- expectations ----
  expect_true(tibble::is_tibble(result))
  expect_equal(ncol(result), 6)  # columns: SourceFile, FileName, DateTimeOriginal, GPSLongitude, GPSLatitude, GPSAltitude
  expect_true(file.exists(tmp_csv))  # CSV should be written

  # Read the saved CSV back in and check it matches column names
  written <- readr::read_csv(tmp_csv, show_col_types = FALSE)
  expect_true(all(names(result) %in% names(written)))
})

test_that("extract_gps_exif returns empty tibble when no files found", {
  tmp_empty <- file.path(tempdir(), "photos_empty")
  dir.create(tmp_empty, showWarnings = FALSE)

  result <- extract_gps_exif(img_dir = tmp_empty)

  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
})
