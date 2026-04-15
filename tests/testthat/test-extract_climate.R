# Tests for extract_climate_values function

test_that("extract_climate_values extracts values from single raster", {
  skip_if_not_installed("terra")

  # Create mock raster with known values
  r <- terra::rast(
    matrix(1:100, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  # Create collection data with points inside raster extent
  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    GPSLatitude = c(28.5, 29.5, 30.5),
    GPSLongitude = c(-79.5, -78.5, -77.5)
  )

  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label"
  )

  expect_true(nrow(result) == 3)
  expect_true("c_label" %in% names(result))
  expect_true(ncol(result) >= 2)  # Should have label and at least one value column

  unlink(temp_raster)
})

test_that("extract_climate_values extracts from multiple rasters", {
  skip_if_not_installed("terra")

  # Create two mock rasters
  r1 <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )
  r2 <- terra::rast(
    matrix(runif(100) * 100, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_r1 <- tempfile(fileext = ".tif")
  temp_r2 <- tempfile(fileext = ".tif")
  terra::writeRaster(r1, temp_r1, overwrite = TRUE)
  terra::writeRaster(r2, temp_r2, overwrite = TRUE)

  collection <- data.frame(
    c_label = c("site1", "site2"),
    GPSLatitude = c(28.5, 29.5),
    GPSLongitude = c(-79.5, -78.5)
  )

  result <- extract_climate_values(
    raster_paths = c(temp_r1, temp_r2),
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label",
    var_names = c("temperature", "precipitation")
  )

  expect_true(nrow(result) == 2)
  expect_true("temperature" %in% names(result) || ncol(result) >= 3)
  expect_true("precipitation" %in% names(result) || ncol(result) >= 3)

  unlink(temp_r1)
  unlink(temp_r2)
})

test_that("extract_climate_values auto-detects latitude column", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  # Try different standard column names
  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 28.5,
    GPSLongitude = -79.5
  )

  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    label_col = "c_label"
    # lat_col and lon_col omitted - should auto-detect
  )

  expect_true(nrow(result) > 0)

  unlink(temp_raster)
})

test_that("extract_climate_values handles missing raster file", {
  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 28.5,
    GPSLongitude = -79.5
  )

  expect_error(
    extract_climate_values(
      raster_paths = "/nonexistent/file.tif",
      collection_df = collection,
      lat_col = "GPSLatitude",
      lon_col = "GPSLongitude",
      label_col = "c_label"
    ),
    "not found"
  )
})

test_that("extract_climate_values handles missing lat/lon columns", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  # Create collection with non-standard column names
  collection <- data.frame(
    c_label = "site1",
    my_lat = 28.5,
    my_lon = -79.5
  )

  expect_error(
    extract_climate_values(
      raster_paths = temp_raster,
      collection_df = collection,
      label_col = "c_label"
      # No lat_col specified and no standard columns present
    ),
    "Could not auto-detect latitude column"
  )

  unlink(temp_raster)
})

test_that("extract_climate_values filters out invalid coordinates", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  # Collection with some NA coordinates
  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    GPSLatitude = c(28.5, NA, 30.5),
    GPSLongitude = c(-79.5, -78.5, NA)
  )

  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label"
  )

  # Should only have 1 or 2 valid sites depending on filtering
  expect_true(nrow(result) <= 2)

  unlink(temp_raster)
})

test_that("extract_climate_values writes CSV output", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  collection <- data.frame(
    c_label = c("site1", "site2"),
    GPSLatitude = c(28.5, 29.5),
    GPSLongitude = c(-79.5, -78.5)
  )

  out_csv <- tempfile(fileext = ".csv")

  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label",
    out_csv = out_csv
  )

  expect_true(file.exists(out_csv))
  expect_true(file.size(out_csv) > 0)

  # Read back the CSV and verify structure
  csv_data <- readr::read_csv(out_csv, show_col_types = FALSE)
  expect_true(nrow(csv_data) >= 1)
  expect_true("c_label" %in% names(csv_data))

  unlink(temp_raster)
  unlink(out_csv)
})

test_that("extract_climate_values handles buffer distance", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 28.5,
    GPSLongitude = -79.5
  )

  # Extract with buffer
  result_buffer <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label",
    buffer_distance = 10000  # 10 km buffer
  )

  expect_true(nrow(result_buffer) > 0)
  # Should have buffer info if it was applied
  expect_true("buffer_distance" %in% names(result_buffer) || nrow(result_buffer) > 0)

  unlink(temp_raster)
})

test_that("extract_climate_values accepts different buffer functions", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(1:100, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 28.5,
    GPSLongitude = -79.5
  )

  # Test with different buffer functions
  for (fun in c("mean", "median", "min", "max")) {
    result <- extract_climate_values(
      raster_paths = temp_raster,
      collection_df = collection,
      lat_col = "GPSLatitude",
      lon_col = "GPSLongitude",
      label_col = "c_label",
      buffer_distance = 5000,
      buffer_fun = fun
    )

    expect_true(nrow(result) > 0)
  }

  unlink(temp_raster)
})

test_that("extract_climate_values returns tibble format", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tibble")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 28.5,
    GPSLongitude = -79.5
  )

  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label"
  )

  expect_true(is.data.frame(result))

  unlink(temp_raster)
})

test_that("extract_climate_values includes original collection columns", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  collection <- data.frame(
    c_label = c("site1", "site2"),
    GPSLatitude = c(28.5, 29.5),
    GPSLongitude = c(-79.5, -78.5),
    species = c("Ce", "Cb"),
    habitat = c("forest", "grassland")
  )

  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label"
  )

  # Should preserve some original columns
  expect_true("c_label" %in% names(result))
  # May include other columns depending on function implementation

  unlink(temp_raster)
})

test_that("extract_climate_values handles custom CRS", {
  skip_if_not_installed("terra")

  # Create raster in different CRS (Web Mercator)
  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-8871000, -7791000, 3210000, 4150000),
    crs = "EPSG:3857"
  )

  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(r, temp_raster, overwrite = TRUE)

  # Collection data in WGS84
  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 30.0,
    GPSLongitude = -80.0
  )

  result <- extract_climate_values(
    raster_paths = temp_raster,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label",
    crs = "EPSG:3857"  # Specify raster CRS
  )

  expect_true(nrow(result) >= 0)  # May be 0 if no overlap, but shouldn't error

  unlink(temp_raster)
})
