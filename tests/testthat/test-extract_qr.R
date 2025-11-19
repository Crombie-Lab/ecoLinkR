test_that("extract_qr writes CSVs and returns a summary", {
  tmp_img_dir <- file.path(tempdir(), "qr_imgs")
  tmp_out_dir <- file.path(tempdir(), "qr_out")

  dir.create(tmp_img_dir, showWarnings = FALSE)
  dir.create(tmp_out_dir, showWarnings = FALSE)

  # Create dummy PNG files
  files <- c("C-0001.png", "C-0002.png")
  file_paths <- file.path(tmp_img_dir, files)
  file.create(file_paths)

  # Mock opencv functions so we do not depend on real image contents
  testthat::local_mocked_bindings(
    ocv_read = function(path) path,
    ocv_qr_detect = function(img) {
      # Always "detect" a QR with simple points data
      structure(
        "TEST_QR",
        points = data.frame(
          x = c(1, 2, 3, 4),
          y = c(5, 6, 7, 8)
        )
      )
    },
    .package = "opencv"
  )

  # Run function (allowing messages)
  expect_error(
    summary_df <- extract_qr(
      img_dir = tmp_img_dir,
      output_dir = tmp_out_dir
    ),
    NA
  )

  # Check returned object
  expect_s3_class(summary_df, "data.frame")
  expect_equal(nrow(summary_df), length(files))
  expect_true(all(c("file", "qr_text", "detected", "csv_path") %in% names(summary_df)))
  expect_true(all(summary_df$detected))

  # Check that CSV files were written
  expect_true(all(file.exists(summary_df$csv_path)))
})

test_that("extract_qr handles empty directories gracefully", {
  tmp_empty_dir <- file.path(tempdir(), "qr_empty_imgs")
  tmp_out_dir <- file.path(tempdir(), "qr_empty_out")

  dir.create(tmp_empty_dir, showWarnings = FALSE)
  dir.create(tmp_out_dir, showWarnings = FALSE)

  expect_warning(
    summary_df <- extract_qr(
      img_dir = tmp_empty_dir,
      output_dir = tmp_out_dir
    ),
    regexp = "No image files found",
    fixed = TRUE
  )

  expect_s3_class(summary_df, "data.frame")
  expect_equal(nrow(summary_df), 0)
})
