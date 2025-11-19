test_that("join_col_iso runs cleanly with dummy data", {

  # Create temporary directories
  tmp_col_dir <- file.path(tempdir(), "photos_col")
  tmp_iso_dir <- file.path(tempdir(), "photos_iso")
  tmp_out_dir <- file.path(tempdir(), "out")
  dir.create(tmp_col_dir, showWarnings = FALSE)
  dir.create(tmp_iso_dir, showWarnings = FALSE)
  dir.create(tmp_out_dir, showWarnings = FALSE)

  # Create dummy photo files
  dummy_col <- c("C-0001.jpg", "C-0002.png")
  dummy_iso <- c("C-0001_iso.jpg", "C-0002_iso.jpg")
  file.create(file.path(tmp_col_dir, dummy_col))
  file.create(file.path(tmp_iso_dir, dummy_iso))

  # Mock collection and isolation data
  mock_col <- tibble::tibble(
    project = "202411_FLTECH",
    c_label = c("C-0001", "C-0002"),
    landscape = "forest"
  )
  mock_iso <- tibble::tibble(
    project_id = "202411_FLTECH",
    c_label = c("C-0001", "C-0002"),
    proliferation_48 = "yes"
  )

  # Modern testthat mocking
  testthat::local_mocked_bindings(
    gsheet2tbl = function(url) {
      if (grepl("col", url)) mock_col else mock_iso
    },
    .package = "gsheet"
  )

  # Run the function with mock data
  result <- join_col_iso(
    c_url = "col_dummy",
    i_url = "iso_dummy",
    c_photo_dir = tmp_col_dir,
    i_photo_dir = tmp_iso_dir,
    out_dir = tmp_out_dir,
    write_csv = FALSE
  )

  # Expectations
  expect_true(tibble::is_tibble(result))
  expect_true(all(c("raw_col_img_name", "raw_iso_img_name") %in% names(result)))
  expect_equal(nrow(result), 2)
})
