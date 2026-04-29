# Tests for extract_climate function

# ── helpers ──────────────────────────────────────────────────────────────────
make_raster <- function(values = runif(100), ext = terra::ext(-80, -70, 25, 35)) {
  terra::rast(matrix(values, ncol = 10), extent = ext, crs = "EPSG:4326")
}

make_lulc_raster <- function(ext = terra::ext(-80, -70, 25, 35)) {
  # Fill with ESA class 10 (Tree Cover) — a valid known value
  terra::rast(matrix(rep(10L, 100), ncol = 10), extent = ext, crs = "EPSG:4326")
}

base_collection <- function() {
  data.frame(
    c_label      = c("C1", "C2", "C3"),
    GPSLatitude  = c(28.5, 29.5, 30.5),
    GPSLongitude = c(-79.5, -78.5, -77.5)
  )
}

# ── basic extraction ──────────────────────────────────────────────────────────

test_that("extracts values from single raster at points", {
  skip_if_not_installed("terra")

  tmp <- tempfile(fileext = ".tif")
  on.exit(unlink(tmp))
  terra::writeRaster(make_raster(1:100), tmp, overwrite = TRUE)

  result <- extract_climate(
    raster_paths = tmp,
    collection_df = base_collection(),
    lat_col = "GPSLatitude",
    lon_col = "GPSLongitude",
    label_col = "c_label"
  )

  expect_equal(nrow(result), 3)
  expect_true("c_label" %in% names(result))
  expect_true("c_latitude" %in% names(result))
  expect_true("c_longitude" %in% names(result))
})

test_that("extracts from multiple rasters with var_names", {
  skip_if_not_installed("terra")

  tmp1 <- tempfile(fileext = ".tif"); on.exit(unlink(tmp1), add = TRUE)
  tmp2 <- tempfile(fileext = ".tif"); on.exit(unlink(tmp2), add = TRUE)
  terra::writeRaster(make_raster(), tmp1, overwrite = TRUE)
  terra::writeRaster(make_raster(runif(100) * 100), tmp2, overwrite = TRUE)

  result <- extract_climate(
    raster_paths  = c(tmp1, tmp2),
    collection_df = base_collection(),
    lat_col       = "GPSLatitude",
    lon_col       = "GPSLongitude",
    label_col     = "c_label",
    var_names     = c("temperature", "precipitation")
  )

  expect_equal(nrow(result), 3)
  expect_true("temperature"   %in% names(result))
  expect_true("precipitation" %in% names(result))
})

test_that("auto-detects GPSLatitude / GPSLongitude columns", {
  skip_if_not_installed("terra")

  tmp <- tempfile(fileext = ".tif"); on.exit(unlink(tmp))
  terra::writeRaster(make_raster(), tmp, overwrite = TRUE)

  result <- extract_climate(
    raster_paths  = tmp,
    collection_df = base_collection(),
    label_col     = "c_label"
    # lat_col / lon_col omitted
  )

  expect_true(nrow(result) > 0)
})

test_that("returns tibble", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tibble")

  tmp <- tempfile(fileext = ".tif"); on.exit(unlink(tmp))
  terra::writeRaster(make_raster(), tmp, overwrite = TRUE)

  result <- extract_climate(
    raster_paths  = tmp,
    collection_df = base_collection()[1, ],
    lat_col       = "GPSLatitude",
    lon_col       = "GPSLongitude",
    label_col     = "c_label"
  )

  expect_s3_class(result, "tbl_df")
})

# ── filtering & validation ────────────────────────────────────────────────────

test_that("filters out rows with NA coordinates", {
  skip_if_not_installed("terra")

  tmp <- tempfile(fileext = ".tif"); on.exit(unlink(tmp))
  terra::writeRaster(make_raster(), tmp, overwrite = TRUE)

  coll <- data.frame(
    c_label      = c("C1", "C2", "C3"),
    GPSLatitude  = c(28.5, NA,   30.5),
    GPSLongitude = c(-79.5, -78.5, NA)
  )

  result <- extract_climate(
    raster_paths  = tmp,
    collection_df = coll,
    lat_col       = "GPSLatitude",
    lon_col       = "GPSLongitude",
    label_col     = "c_label"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$c_label, "C1")
})

test_that("errors on missing raster file", {
  expect_error(
    extract_climate(
      raster_paths  = "/nonexistent/file.tif",
      collection_df = base_collection(),
      lat_col       = "GPSLatitude",
      lon_col       = "GPSLongitude",
      label_col     = "c_label"
    ),
    "not found"
  )
})

test_that("errors when lat column cannot be auto-detected", {
  skip_if_not_installed("terra")

  tmp <- tempfile(fileext = ".tif"); on.exit(unlink(tmp))
  terra::writeRaster(make_raster(), tmp, overwrite = TRUE)

  coll <- data.frame(c_label = "C1", my_lat = 28.5, my_lon = -79.5)

  expect_error(
    extract_climate(
      raster_paths  = tmp,
      collection_df = coll,
      label_col     = "c_label"
    ),
    "Could not auto-detect latitude column"
  )
})

test_that("errors when var_names length mismatches raster_paths", {
  skip_if_not_installed("terra")

  tmp <- tempfile(fileext = ".tif"); on.exit(unlink(tmp))
  terra::writeRaster(make_raster(), tmp, overwrite = TRUE)

  expect_error(
    extract_climate(
      raster_paths  = tmp,
      collection_df = base_collection(),
      lat_col       = "GPSLatitude",
      lon_col       = "GPSLongitude",
      label_col     = "c_label",
      var_names     = c("a", "b")  # 2 names for 1 raster
    ),
    "Length of var_names"
  )
})

# ── buffer extraction ─────────────────────────────────────────────────────────

test_that("buffer extraction appends buffer_distance and buffer_fun columns", {
  skip_if_not_installed("terra")

  tmp <- tempfile(fileext = ".tif"); on.exit(unlink(tmp))
  terra::writeRaster(make_raster(1:100), tmp, overwrite = TRUE)

  result <- extract_climate(
    raster_paths    = tmp,
    collection_df   = base_collection()[1, ],
    lat_col         = "GPSLatitude",
    lon_col         = "GPSLongitude",
    label_col       = "c_label",
    buffer_distance = 10000
  )

  expect_true("buffer_distance" %in% names(result))
  expect_true("buffer_fun"      %in% names(result))
  expect_equal(result$buffer_distance, 10000)
})

test_that("buffer_fun options all run without error", {
  skip_if_not_installed("terra")

  tmp <- tempfile(fileext = ".tif"); on.exit(unlink(tmp))
  terra::writeRaster(make_raster(1:100), tmp, overwrite = TRUE)

  for (fun in c("mean", "median", "min", "max", "sd")) {
    expect_no_error(
      extract_climate(
        raster_paths    = tmp,
        collection_df   = base_collection()[1, ],
        lat_col         = "GPSLatitude",
        lon_col         = "GPSLongitude",
        label_col       = "c_label",
        buffer_distance = 5000,
        buffer_fun      = fun
      )
    )
  }
})

# ── CSV output ────────────────────────────────────────────────────────────────

test_that("writes CSV when out_csv is specified", {
  skip_if_not_installed("terra")
  skip_if_not_installed("readr")

  tmp     <- tempfile(fileext = ".tif"); on.exit(unlink(tmp),     add = TRUE)
  out_csv <- tempfile(fileext = ".csv"); on.exit(unlink(out_csv), add = TRUE)
  terra::writeRaster(make_raster(), tmp, overwrite = TRUE)

  extract_climate(
    raster_paths  = tmp,
    collection_df = base_collection(),
    lat_col       = "GPSLatitude",
    lon_col       = "GPSLongitude",
    label_col     = "c_label",
    out_csv       = out_csv
  )

  expect_true(file.exists(out_csv))
  csv_back <- readr::read_csv(out_csv, show_col_types = FALSE)
  expect_true("c_label" %in% names(csv_back))
  expect_equal(nrow(csv_back), 3)
})

# ── LULC extraction ───────────────────────────────────────────────────────────

test_that("lulc_path adds lulc_value and lulc_label columns", {
  skip_if_not_installed("terra")

  tmp_clim <- tempfile(fileext = ".tif"); on.exit(unlink(tmp_clim), add = TRUE)
  tmp_lulc <- tempfile(fileext = ".tif"); on.exit(unlink(tmp_lulc), add = TRUE)
  terra::writeRaster(make_raster(),       tmp_clim, overwrite = TRUE)
  terra::writeRaster(make_lulc_raster(),  tmp_lulc, overwrite = TRUE)

  result <- extract_climate(
    raster_paths  = tmp_clim,
    collection_df = base_collection(),
    lat_col       = "GPSLatitude",
    lon_col       = "GPSLongitude",
    label_col     = "c_label",
    lulc_path     = tmp_lulc
  )

  expect_true("lulc_value" %in% names(result))
  expect_true("lulc_label" %in% names(result))
  # All pixels are 10 (Tree Cover)
  expect_true(all(result$lulc_value == 10L, na.rm = TRUE))
  expect_true(all(result$lulc_label == "Tree Cover", na.rm = TRUE))
})

test_that("lulc_col renames output columns", {
  skip_if_not_installed("terra")

  tmp_clim <- tempfile(fileext = ".tif"); on.exit(unlink(tmp_clim), add = TRUE)
  tmp_lulc <- tempfile(fileext = ".tif"); on.exit(unlink(tmp_lulc), add = TRUE)
  terra::writeRaster(make_raster(),      tmp_clim, overwrite = TRUE)
  terra::writeRaster(make_lulc_raster(), tmp_lulc, overwrite = TRUE)

  result <- extract_climate(
    raster_paths  = tmp_clim,
    collection_df = base_collection()[1, ],
    lat_col       = "GPSLatitude",
    lon_col       = "GPSLongitude",
    label_col     = "c_label",
    lulc_path     = tmp_lulc,
    lulc_col      = "land_cover"
  )

  expect_true("land_cover_value" %in% names(result))
  expect_true("land_cover_label" %in% names(result))
  expect_false("lulc_value" %in% names(result))
})

test_that("lulc extraction with buffer uses modal class", {
  skip_if_not_installed("terra")

  tmp_clim <- tempfile(fileext = ".tif"); on.exit(unlink(tmp_clim), add = TRUE)
  tmp_lulc <- tempfile(fileext = ".tif"); on.exit(unlink(tmp_lulc), add = TRUE)
  terra::writeRaster(make_raster(),      tmp_clim, overwrite = TRUE)
  terra::writeRaster(make_lulc_raster(), tmp_lulc, overwrite = TRUE)

  result <- extract_climate(
    raster_paths    = tmp_clim,
    collection_df   = base_collection()[1, ],
    lat_col         = "GPSLatitude",
    lon_col         = "GPSLongitude",
    label_col       = "c_label",
    buffer_distance = 10000,
    lulc_path       = tmp_lulc
  )

  expect_true("lulc_value" %in% names(result))
  expect_true("lulc_label" %in% names(result))
  expect_equal(result$lulc_value, 10L)
  expect_equal(result$lulc_label, "Tree Cover")
})

test_that("missing lulc_path warns and skips rather than erroring", {
  skip_if_not_installed("terra")

  tmp_clim <- tempfile(fileext = ".tif"); on.exit(unlink(tmp_clim), add = TRUE)
  terra::writeRaster(make_raster(), tmp_clim, overwrite = TRUE)

  expect_warning(
    result <- extract_climate(
      raster_paths  = tmp_clim,
      collection_df = base_collection()[1, ],
      lat_col       = "GPSLatitude",
      lon_col       = "GPSLongitude",
      label_col     = "c_label",
      lulc_path     = "/nonexistent/lulc.tif"
    ),
    "land cover extraction skipped"
  )

  expect_false("lulc_value" %in% names(result))
})

# ── CRS reprojection ──────────────────────────────────────────────────────────

test_that("reprojects raster from EPSG:3857 without error", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    matrix(runif(100), ncol = 10),
    extent = terra::ext(-8871000, -7791000, 3210000, 4150000),
    crs    = "EPSG:3857"
  )
  tmp <- tempfile(fileext = ".tif"); on.exit(unlink(tmp))
  terra::writeRaster(r, tmp, overwrite = TRUE)

  expect_no_error(
    extract_climate(
      raster_paths  = tmp,
      collection_df = data.frame(c_label = "C1", GPSLatitude = 30.0, GPSLongitude = -80.0),
      lat_col       = "GPSLatitude",
      lon_col       = "GPSLongitude",
      label_col     = "c_label"
    )
  )
})
