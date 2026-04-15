# Tests for plot_collections_raster function

test_that("plot_collections_raster creates a map with single raster", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  # Create a mock raster with known values
  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  # Create collection data with coordinates
  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    c_latitude = c(28.5, 29.5, 30.5),
    c_longitude = c(-79.5, -78.5, -77.5)
  )

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster creates a map with multiple rasters", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  # Create two mock rasters
  r1 <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )
  r2 <- terra::rast(
    matrix(100 + (1:100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = c("site1", "site2"),
    c_latitude = c(28.5, 29.5),
    c_longitude = c(-79.5, -78.5)
  )

  raster_list <- list(Temperature = r1, Precipitation = r2)
  map <- plot_collections_raster(
    raster_list = raster_list,
    collection_df = collection
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster auto-detects latitude/longitude columns", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  # Test with c_latitude/c_longitude
  collection1 <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  map1 <- plot_collections_raster(
    raster_list = r,
    collection_df = collection1
  )
  expect_s3_class(map1, "leaflet")

  # Test with collection_latitude/collection_longitude
  collection2 <- data.frame(
    c_label = "site1",
    collection_latitude = 29.0,
    collection_longitude = -79.0
  )

  map2 <- plot_collections_raster(
    raster_list = r,
    collection_df = collection2
  )
  expect_s3_class(map2, "leaflet")

  # Test with lat/lon
  collection3 <- data.frame(
    c_label = "site1",
    lat = 29.0,
    lon = -79.0
  )

  map3 <- plot_collections_raster(
    raster_list = r,
    collection_df = collection3
  )
  expect_s3_class(map3, "leaflet")
})

test_that("plot_collections_raster works with custom lat/lon columns", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    GPSLatitude = 29.0,
    GPSLongitude = -79.0
  )

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection,
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude"
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster handles species coloring", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    c_latitude = c(28.5, 29.5, 30.5),
    c_longitude = c(-79.5, -78.5, -77.5),
    species_id = c("Ce", "Cb", "Om")
  )

  species_colors <- c(Ce = "blue", Cb = "orange", Om = "red")

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection,
    species_col = "species_id",
    species_colors = species_colors
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster handles species coloring with defaults", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = c("site1", "site2"),
    c_latitude = c(28.5, 29.5),
    c_longitude = c(-79.5, -78.5),
    species_id = c("Ce", "Cb")
  )

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection,
    species_col = "species_id"
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster saves HTML output", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  temp_dir <- tempdir()
  out_html <- file.path(temp_dir, "test_map.html")

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection,
    out_html = out_html
  )

  expect_true(file.exists(out_html))
  unlink(out_html)
})

test_that("plot_collections_raster handles raster CRS reprojection", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  # Create raster in UTM
  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(400000, 500000, 3100000, 3200000),
    crs = "EPSG:32617"  # UTM Zone 17N
  )

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster handles custom title", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection,
    title = "My Custom Title"
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster handles show_rasters parameter", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r1 <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )
  r2 <- terra::rast(
    matrix(100 + (1:100), ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  raster_list <- list(Temperature = r1, Precipitation = r2)

  # Show only Temperature by default
  map <- plot_collections_raster(
    raster_list = raster_list,
    collection_df = collection,
    show_rasters = "Temperature"
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster filters sites with invalid coordinates", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  # Include sites with NA coordinates
  collection <- data.frame(
    c_label = c("site1", "site2", "site3"),
    c_latitude = c(28.5, NA, 30.5),
    c_longitude = c(-79.5, -78.5, NA)
  )

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster handles opacity parameter", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  map <- plot_collections_raster(
    raster_list = r,
    collection_df = collection,
    opacity = 0.3
  )

  expect_s3_class(map, "leaflet")
})

test_that("plot_collections_raster handles different color palettes", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  for (pal in c("viridis", "magma", "plasma", "inferno")) {
    map <- plot_collections_raster(
      raster_list = r,
      collection_df = collection,
      palette = pal
    )
    expect_s3_class(map, "leaflet")
  }
})

# Error handling tests
test_that("plot_collections_raster errors with empty raster list", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  expect_error(
    plot_collections_raster(raster_list = list(), collection_df = collection),
    "at least one raster"
  )
})

test_that("plot_collections_raster errors with non-SpatRaster", {
  skip_if_not_installed("leaflet")

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  expect_error(
    plot_collections_raster(
      raster_list = list(matrix(1:100, ncol = 10)),
      collection_df = collection
    ),
    "not a SpatRaster"
  )
})

test_that("plot_collections_raster errors with non-data frame", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  expect_error(
    plot_collections_raster(
      raster_list = r,
      collection_df = list(c_label = "site1")
    ),
    "must be a data frame"
  )
})

test_that("plot_collections_raster errors with missing lat/lon columns", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    other_col = 29.0
  )

  expect_error(
    plot_collections_raster(
      raster_list = r,
      collection_df = collection
    ),
    "Could not auto-detect latitude"
  )
})

test_that("plot_collections_raster errors with no valid coordinates", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = c("site1", "site2"),
    c_latitude = c(NA, NA),
    c_longitude = c(NA, NA)
  )

  expect_error(
    plot_collections_raster(
      raster_list = r,
      collection_df = collection
    ),
    "No valid lat/lon coordinates"
  )
})

test_that("plot_collections_raster returns leaflet object invisibly", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r <- terra::rast(
    matrix(20:119, ncol = 10),
    extent = terra::ext(-80, -70, 25, 35),
    crs = "EPSG:4326"
  )

  collection <- data.frame(
    c_label = "site1",
    c_latitude = 29.0,
    c_longitude = -79.0
  )

  result <- plot_collections_raster(
    raster_list = r,
    collection_df = collection
  )

  expect_s3_class(result, "leaflet")
})
