# Tests for join_climate_to_collection function

test_that("join_climate_to_collection joins data correctly with left join", {
  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    species_id = c("Ce", "Cb", "Om"),
    location = c("forest", "grassland", "wetland")
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3),
    mean_precip = c(150.2, 160.5)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )

  expect_equal(nrow(result), 3)
  expect_true("mean_temp" %in% names(result))
  expect_true("mean_precip" %in% names(result))
  expect_true(is.na(result$mean_temp[3]))  # site3 has no climate data
})

test_that("join_climate_to_collection performs inner join correctly", {
  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    species_id = c("Ce", "Cb", "Om")
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "inner",
    verbose = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("site1", "site2") %in% result$c_label))
  expect_false("site3" %in% result$c_label)
})

test_that("join_climate_to_collection performs full join correctly", {
  collection <- data.frame(
    c_label = c("site1", "site2"),
    species_id = c("Ce", "Cb")
  )

  climate <- data.frame(
    c_label = c("site2", "site3"),
    mean_temp = c(26.3, 27.1)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "full",
    verbose = FALSE
  )

  expect_equal(nrow(result), 3)
  expect_true(all(c("site1", "site2", "site3") %in% result$c_label))
})

test_that("join_climate_to_collection handles duplicate keys in climate_df", {
  collection <- data.frame(
    c_label = c("site1", "site2"),
    species_id = c("Ce", "Cb")
  )

  # Climate data has duplicate keys
  climate <- data.frame(
    c_label = c("site1", "site1", "site2"),
    mean_temp = c(25.5, 25.6, 26.3),
    source = c("2024", "2025", "2024")
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )

  # Should keep only first occurrence
  expect_equal(nrow(result), 2)
})

test_that("join_climate_to_collection validates input data frames", {
  not_df <- c("a", "b", "c")

  expect_error(
    join_climate_to_collection(
      collection_df = not_df,
      climate_df = data.frame(),
      by = "c_label"
    ),
    "must be a data frame"
  )

  expect_error(
    join_climate_to_collection(
      collection_df = data.frame(c_label = "site1"),
      climate_df = not_df,
      by = "c_label"
    ),
    "must be a data frame"
  )
})

test_that("join_climate_to_collection validates join column exists", {
  collection <- data.frame(
    c_label = c("site1", "site2"),
    species_id = c("Ce", "Cb")
  )

  climate <- data.frame(
    climate_id = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  expect_error(
    join_climate_to_collection(
      collection_df = collection,
      climate_df = climate,
      by = "c_label",
      join_type = "left"
    ),
    "not found in climate_df"
  )
})

test_that("join_climate_to_collection validates join type", {
  collection <- data.frame(c_label = "site1", species_id = "Ce")
  climate <- data.frame(c_label = "site1", mean_temp = 25.5)

  expect_error(
    join_climate_to_collection(
      collection_df = collection,
      climate_df = climate,
      by = "c_label",
      join_type = "invalid"
    ),
    "'arg' should be one of"
  )
})

test_that("join_climate_to_collection writes CSV output", {
  collection <- data.frame(
    c_label = c("site1", "site2"),
    species_id = c("Ce", "Cb")
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  out_csv <- tempfile(fileext = ".csv")

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    out_csv = out_csv,
    verbose = FALSE
  )

  expect_true(file.exists(out_csv))
  expect_true(file.size(out_csv) > 0)

  # Read back and verify
  read_back <- readr::read_csv(out_csv, show_col_types = FALSE)
  expect_equal(nrow(read_back), nrow(result))

  unlink(out_csv)
})

test_that("join_climate_to_collection prints diagnostics when verbose = TRUE", {
  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    species_id = c("Ce", "Cb", "Om")
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  expect_message(
    join_climate_to_collection(
      collection_df = collection,
      climate_df = climate,
      by = "c_label",
      join_type = "left",
      verbose = TRUE
    ),
    "Join diagnostics"
  )
})

test_that("join_climate_to_collection preserves all collection columns", {
  collection <- data.frame(
    c_label = c("site1", "site2"),
    species_id = c("Ce", "Cb"),
    habitat = c("forest", "grassland"),
    elevation = c(100, 200)
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )

  # Check all original columns are present
  expect_true("c_label" %in% names(result))
  expect_true("species_id" %in% names(result))
  expect_true("habitat" %in% names(result))
  expect_true("elevation" %in% names(result))
  expect_true("mean_temp" %in% names(result))
})

test_that("join_climate_to_collection handles multiple isolates per site", {
  # Multiple rows for same collection (e.g., different isolations)
  collection <- data.frame(
    c_label = c("site1", "site1", "site1", "site2"),
    isolation_id = c("A", "B", "C", "A"),
    species_id = c("Ce", "Ce", "Ce", "Cb")
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )

  expect_equal(nrow(result), 4)  # All 4 collection rows preserved
  expect_true(all(result$mean_temp[1:3] == 25.5))  # site1 isolates get same climate
  expect_equal(result$mean_temp[4], 26.3)  # site2 gets different climate
})

test_that("join_climate_to_collection handles NA values in join column", {
  collection <- data.frame(
    c_label = c("site1", "site2", NA),
    species_id = c("Ce", "Cb", "Om")
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )

  expect_equal(nrow(result), 3)
  expect_true(is.na(result$mean_temp[3]))
})

test_that("join_climate_to_collection works with different join column names", {
  collection <- data.frame(
    site_id = c("site1", "site2"),
    species_id = c("Ce", "Cb")
  )

  climate <- data.frame(
    site_id = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "site_id",
    join_type = "left",
    verbose = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_true("mean_temp" %in% names(result))
})

test_that("join_climate_to_collection returns data frame with correct structure", {
  collection <- data.frame(
    c_label = c("site1", "site2"),
    species_id = c("Ce", "Cb"),
    stringsAsFactors = FALSE
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3),
    mean_precip = c(150.2, 160.5),
    stringsAsFactors = FALSE
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 4)  # c_label, species_id, mean_temp, mean_precip
})

test_that("join_climate_to_collection handles empty climate data frame", {
  collection <- data.frame(
    c_label = c("site1", "site2"),
    species_id = c("Ce", "Cb")
  )

  climate <- data.frame(
    c_label = character(0),
    mean_temp = numeric(0)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$mean_temp)))
})

test_that("join_climate_to_collection handles empty collection data frame", {
  collection <- data.frame(
    c_label = character(0),
    species_id = character(0)
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  result <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )

  expect_equal(nrow(result), 0)
})

