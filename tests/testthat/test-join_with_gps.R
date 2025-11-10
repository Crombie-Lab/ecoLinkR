test_that("join_with_gps runs cleanly and merges correctly", {
  # ---- create temporary dummy data ----
  tmp_dir <- tempdir()
  coliso_path <- file.path(tmp_dir, "coliso.csv")
  gps_path <- file.path(tmp_dir, "gps.csv")
  out_path <- file.path(tmp_dir, "joined_out.csv")

  # Fake collection/isolation dataset
  coliso_df <- tibble::tibble(
    raw_col_img_name = c("C-0001.png", "C-0002.png", "C-0003.png"),
    sample_id = c("S1", "S2", "S3"),
    project = "test_project"
  )

  # Fake GPS dataset (with matching FileName values)
  gps_df <- tibble::tibble(
    FileName = c("C-0001.png", "C-0002.png", "C-9999.png"),
    GPSLatitude = c(28.1, 29.2, 99.9),
    GPSLongitude = c(-80.6, -81.1, 0.0)
  )

  # Write to disk
  readr::write_csv(coliso_df, coliso_path)
  readr::write_csv(gps_df, gps_path)

  # ---- run the function (allowing messages) ----
  expect_error(
    result <- join_with_gps(
      coliso_csv = coliso_path,
      gps_csv = gps_path,
      out_csv = out_path,
      keep_unmatched = TRUE
    ),
    NA
  )

  # ---- check outputs ----
  expect_true(file.exists(out_path))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("GPSLatitude", "GPSLongitude") %in% names(result)))

  # The first two rows should have GPS data; third should be NA
  expect_false(all(is.na(result$GPSLatitude[1:2])))
  expect_true(is.na(result$GPSLatitude[3]))

  # ---- check that normalization worked ----
  # This should still work if the GPS names have full paths
  gps_df2 <- gps_df
  gps_df2$FileName <- file.path("/Users/test/photos/", gps_df2$FileName)
  gps_path2 <- file.path(tmp_dir, "gps_fullpath.csv")
  readr::write_csv(gps_df2, gps_path2)

  expect_error(
    result2 <- join_with_gps(
      coliso_csv = coliso_path,
      gps_csv = gps_path2
    ),
    NA
  )

  expect_true(all(c("GPSLatitude", "GPSLongitude") %in% names(result2)))
  expect_equal(sum(!is.na(result2$GPSLatitude)), 2)

  # ---- check manual join key works ----
  expect_error(
    result3 <- join_with_gps(
      coliso_csv = coliso_path,
      gps_csv = gps_path,
      join_key = c("raw_col_img_name" = "FileName")
    ),
    NA
  )
  expect_true(all(c("GPSLatitude", "GPSLongitude") %in% names(result3)))

  # ---- check keep_unmatched = FALSE ----
  expect_error(
    result4 <- join_with_gps(
      coliso_csv = coliso_path,
      gps_csv = gps_path,
      keep_unmatched = FALSE
    ),
    NA
  )

  # The filtered version should have fewer rows (only matched)
  expect_lt(nrow(result4), nrow(coliso_df))
  expect_equal(sum(!is.na(result4$GPSLatitude)), nrow(result4))

  # ---- cleanup ----
  unlink(c(coliso_path, gps_path, gps_path2, out_path))
})
