# Test fetch_climate function

test_that("fetch_climate validates input", {
  # Test that function requires dates when use_prism=TRUE
  expect_error(
    fetch_climate(
      variable = "tmean",
      use_prism = TRUE,
      raster_dir = tempdir()
    ),
    "start_date and end_date are required"
  )

  # Test that function requires raster_dir when use_prism=FALSE
  expect_error(
    fetch_climate(
      variable = "tmean",
      use_prism = FALSE
    ),
    "raster_dir must be provided"
  )

  # Test that function rejects invalid directories when use_prism=FALSE
  expect_error(
    fetch_climate(
      variable = "tmean",
      raster_dir = "/nonexistent/path/xyz",
      use_prism = FALSE
    ),
    "raster_dir not found"
  )
})

test_that("fetch_climate validates variables", {
  # Test that invalid variable throws error
  expect_error(
    fetch_climate(
      variable = "invalid_var",
      raster_dir = tempdir(),
      use_prism = FALSE
    )
  )

  # Test that at least one variable is required
  expect_error(
    fetch_climate(
      variable = character(0),
      raster_dir = tempdir(),
      use_prism = FALSE
    ),
    "At least one variable must be specified"
  )
})

test_that("fetch_climate handles date formats", {
  # Test invalid date format - should throw an error
  # (either from as.Date() or our custom validation)
  expect_error(
    fetch_climate(
      variable = "tmean",
      start_date = "invalid",
      end_date = "2025-02-01",
      use_prism = TRUE,
      raster_dir = tempdir()
    )
  )
})


# Test extract_climate_values function

test_that("extract_climate_values validates input", {
  skip_if_not_installed("terra")

  # Create mock raster and save to temporary file
  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )
  
  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  # Create mock collection data
  collection <- data.frame(
    c_label = c("site1", "site2"),
    GPSLatitude = c(28.5, 29.5),
    GPSLongitude = c(-80.5, -79.5)
  )

  # Test with valid inputs
  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label"
  )

  expect_true(nrow(result) > 0)
  expect_true("c_label" %in% names(result))
  
  unlink(temp_raster)
})

test_that("extract_climate_values auto-detects columns", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )
  
  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  # Use standard column names for auto-detection
  collection <- data.frame(
    c_label = c("site1", "site2"),
    GPSLatitude = c(28.5, 29.5),
    GPSLongitude = c(-80.5, -79.5)
  )

  # Should work without specifying lat_col/lon_col
  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    label_col = "c_label"
  )

  expect_true(nrow(result) > 0)
  
  unlink(temp_raster)
})

test_that("extract_climate_values handles missing rasters", {
  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 28.5,
    GPSLongitude = -80.5
  )

  expect_error(
    extract_climate_values(
      raster_paths = "/nonexistent/raster.tif",
      collection_df = collection,
      lat_col = "GPSLatitude",
      lon_col = "GPSLongitude"
    ),
    "not found"
  )
})


# Test join_climate_to_collection function

test_that("join_climate_to_collection joins data correctly", {
  collection <- data.frame(
    c_label = c("site1", "site2"),
    species_id = c("Ce", "Cb")
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

  expect_true(all(c("c_label", "species_id", "mean_temp") %in% names(result)))
  expect_equal(nrow(result), 2)
})

test_that("join_climate_to_collection handles different join types", {
  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    species_id = c("Ce", "Cb", "Om")
  )

  climate <- data.frame(
    c_label = c("site1", "site2"),
    mean_temp = c(25.5, 26.3)
  )

  # Left join should keep all collection rows
  result_left <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "left",
    verbose = FALSE
  )
  expect_equal(nrow(result_left), 3)

  # Inner join should keep only matching rows
  result_inner <- join_climate_to_collection(
    collection_df = collection,
    climate_df = climate,
    by = "c_label",
    join_type = "inner",
    verbose = FALSE
  )
  expect_equal(nrow(result_inner), 2)
})


# Test plot_collections_raster function

test_that("plot_collections_raster creates a map", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  # Create mock raster
  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  # Create mock collection data
  collection <- data.frame(
    c_label = c("site1", "site2"),
    GPSLatitude = c(28.5, 29.5),
    GPSLongitude = c(-80.5, -79.5)
  )

  # Create map
  map <- plot_collections_raster(
    raster_list = list(Temperature = r),
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label"
  )

  expect_true(inherits(map, "leaflet"))
})

test_that("plot_collections_raster auto-detects columns", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = c("site1", "site2"),
    GPSLatitude = c(28.5, 29.5),
    GPSLongitude = c(-80.5, -79.5),
    species_id = c("Ce", "Cb")
  )

  # Should work without specifying lat_col/lon_col
  map <- plot_collections_raster(
    raster_list = list(r),
    collection_df = collection,
    label_col = "c_label"
  )

  expect_true(inherits(map, "leaflet"))
})

test_that("plot_collections_raster handles single raster", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 28.5,
    GPSLongitude = -80.5
  )

  # Pass single raster instead of list
  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude"
  )

  expect_true(inherits(map, "leaflet"))
})
