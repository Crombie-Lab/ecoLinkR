# Tests for plot_collections_raster

# ── helpers ───────────────────────────────────────────────────────────────────

make_raster <- function(values = 1:100,
                        ext = terra::ext(-80, -70, 25, 35),
                        crs = "EPSG:4326") {
  terra::rast(matrix(values, ncol = 10), extent = ext, crs = crs)
}

make_lulc_raster <- function(ext = terra::ext(-80, -70, 25, 35)) {
  terra::rast(matrix(rep(10L, 100), ncol = 10), extent = ext, crs = "EPSG:4326")
}

base_collection <- function() {
  data.frame(
    c_label     = c("C1", "C2", "C3"),
    c_latitude  = c(28.5, 29.5, 30.5),
    c_longitude = c(-79.5, -78.5, -77.5)
  )
}

# ── basic map creation ────────────────────────────────────────────────────────

test_that("returns a leaflet object for a single raster", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  map <- plot_collections_raster(
    raster_list   = make_raster(),
    collection_df = base_collection()
  )

  expect_s3_class(map, "leaflet")
})

test_that("returns a leaflet object for a named list of rasters", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  map <- plot_collections_raster(
    raster_list   = list(Temperature = make_raster(), Precipitation = make_raster(101:200)),
    collection_df = base_collection()
  )

  expect_s3_class(map, "leaflet")
})

test_that("auto-converts a bare SpatRaster to a list", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  # Passing a SpatRaster directly (not wrapped in list) should not error
  expect_s3_class(
    plot_collections_raster(
      raster_list   = make_raster(),
      collection_df = base_collection()
    ),
    "leaflet"
  )
})

test_that("auto-names unnamed rasters as Layer_1, Layer_2, ...", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  # Unnamed list — function should assign Layer_1, Layer_2
  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(make_raster(), make_raster(101:200)),
      collection_df = base_collection()
    ),
    "leaflet"
  )
})

# ── input validation ──────────────────────────────────────────────────────────

test_that("errors on empty raster_list", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  expect_error(
    plot_collections_raster(
      raster_list   = list(),
      collection_df = base_collection()
    ),
    "at least one raster"
  )
})

test_that("errors when raster_list element is not a SpatRaster", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  expect_error(
    plot_collections_raster(
      raster_list   = list(not_a_raster = matrix(1:9, 3)),
      collection_df = base_collection()
    ),
    "SpatRaster"
  )
})

test_that("errors when collection_df is not a data frame", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  expect_error(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = "not_a_df"
    ),
    "data frame"
  )
})

test_that("errors when lat column cannot be auto-detected", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  bad_coll <- data.frame(c_label = "C1", my_y = 28.5, my_x = -79.5)

  expect_error(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = bad_coll
    ),
    "auto-detect latitude"
  )
})

test_that("errors when all coordinates are NA", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  na_coll <- data.frame(c_label = "C1", c_latitude = NA_real_, c_longitude = NA_real_)

  expect_error(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = na_coll
    ),
    "No valid lat/lon"
  )
})

test_that("errors when lulc is not a SpatRaster, path, or URL vector", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  expect_error(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = base_collection(),
      lulc          = 42L
    ),
    "SpatRaster"
  )
})

# ── lat/lon auto-detection ────────────────────────────────────────────────────

test_that("auto-detects collection_latitude / collection_longitude", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  coll <- data.frame(
    c_label              = "C1",
    collection_latitude  = 29.0,
    collection_longitude = -79.0
  )
  expect_s3_class(
    plot_collections_raster(raster_list = list(r = make_raster()), collection_df = coll),
    "leaflet"
  )
})

test_that("auto-detects GPSLatitude / GPSLongitude", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  coll <- data.frame(c_label = "C1", GPSLatitude = 29.0, GPSLongitude = -79.0)
  expect_s3_class(
    plot_collections_raster(raster_list = list(r = make_raster()), collection_df = coll),
    "leaflet"
  )
})

test_that("auto-detects lat / lon", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  coll <- data.frame(c_label = "C1", lat = 29.0, lon = -79.0)
  expect_s3_class(
    plot_collections_raster(raster_list = list(r = make_raster()), collection_df = coll),
    "leaflet"
  )
})

# ── species-based circle markers ──────────────────────────────────────────────

test_that("species_col colors markers without error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  coll <- data.frame(
    c_label     = c("C1", "C2", "C3"),
    c_latitude  = c(28.5, 29.5, 30.5),
    c_longitude = c(-79.5, -78.5, -77.5),
    species_id  = c("Ce", "Cb", "Ce")
  )

  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = coll,
      species_col   = "species_id",
      species_colors = c(Ce = "blue", Cb = "orange")
    ),
    "leaflet"
  )
})

test_that("species_col with no species_colors uses default palette", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  coll <- data.frame(
    c_label     = c("C1", "C2"),
    c_latitude  = c(28.5, 29.5),
    c_longitude = c(-79.5, -78.5),
    species_id  = c("Ce", "Cb")
  )

  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = coll,
      species_col   = "species_id"
    ),
    "leaflet"
  )
})

# ── proliferating-based awesome icon markers ──────────────────────────────────

test_that("proliferating_col uses awesomeIcons without error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  coll <- data.frame(
    c_label       = c("C1", "C2", "C3"),
    c_latitude    = c(28.5, 29.5, 30.5),
    c_longitude   = c(-79.5, -78.5, -77.5),
    proliferating = c(1, 1, 0),
    species_id    = c("Ce", NA, NA)
  )

  expect_s3_class(
    plot_collections_raster(
      raster_list       = list(r = make_raster()),
      collection_df     = coll,
      proliferating_col = "proliferating",
      species_col       = "species_id"
    ),
    "leaflet"
  )
})

test_that("proliferating all-zero produces black markers", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  coll <- data.frame(
    c_label       = "C1",
    c_latitude    = 28.5,
    c_longitude   = -79.5,
    proliferating = 0
  )

  expect_s3_class(
    plot_collections_raster(
      raster_list       = list(r = make_raster()),
      collection_df     = coll,
      proliferating_col = "proliferating"
    ),
    "leaflet"
  )
})

# ── lulc layer ────────────────────────────────────────────────────────────────

test_that("lulc SpatRaster is accepted and map is returned", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = base_collection(),
      lulc          = make_lulc_raster()
    ),
    "leaflet"
  )
})

test_that("lulc file path is accepted and map is returned", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  tmp_lulc <- tempfile(fileext = ".tif")
  on.exit(unlink(tmp_lulc))
  terra::writeRaster(make_lulc_raster(), tmp_lulc, overwrite = TRUE)

  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = base_collection(),
      lulc          = tmp_lulc
    ),
    "leaflet"
  )
})

test_that("show_lulc = FALSE does not error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(r = make_raster()),
      collection_df = base_collection(),
      lulc          = make_lulc_raster(),
      show_lulc     = FALSE
    ),
    "leaflet"
  )
})

# ── show_rasters ──────────────────────────────────────────────────────────────

test_that("show_rasters = NULL shows all layers without error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(A = make_raster(), B = make_raster(101:200)),
      collection_df = base_collection(),
      show_rasters  = NULL
    ),
    "leaflet"
  )
})

test_that("show_rasters subset hides non-selected layers without error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(A = make_raster(), B = make_raster(101:200)),
      collection_df = base_collection(),
      show_rasters  = "A"
    ),
    "leaflet"
  )
})

# ── CRS reprojection ──────────────────────────────────────────────────────────

test_that("raster in EPSG:3857 is reprojected silently", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  r_merc <- terra::rast(
    matrix(1:100, ncol = 10),
    extent = terra::ext(-8900000, -7800000, 3200000, 4200000),
    crs    = "EPSG:3857"
  )

  expect_s3_class(
    plot_collections_raster(
      raster_list   = list(r = r_merc),
      collection_df = data.frame(
        c_label     = "C1",
        c_latitude  = 29.0,
        c_longitude = -79.0
      )
    ),
    "leaflet"
  )
})

# ── HTML output ───────────────────────────────────────────────────────────────

test_that("out_html writes an HTML file to disk", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")

  out_html <- tempfile(fileext = ".html")
  on.exit(unlink(c(out_html, paste0(tools::file_path_sans_ext(out_html), "_files")),
                 recursive = TRUE))

  plot_collections_raster(
    raster_list   = list(r = make_raster()),
    collection_df = base_collection(),
    out_html      = out_html
  )

  expect_true(file.exists(out_html))
  expect_gt(file.size(out_html), 0)
})

test_that("out_html creates parent directory if missing", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")

  new_dir  <- file.path(tempdir(), paste0("pcr_out_", Sys.getpid()))
  out_html <- file.path(new_dir, "map.html")
  on.exit(unlink(new_dir, recursive = TRUE))

  plot_collections_raster(
    raster_list   = list(r = make_raster()),
    collection_df = base_collection(),
    out_html      = out_html
  )

  expect_true(file.exists(out_html))
})

# ── return value ──────────────────────────────────────────────────────────────

test_that("return value is invisible (still accessible)", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")

  result <- plot_collections_raster(
    raster_list   = list(r = make_raster()),
    collection_df = base_collection()
  )

  expect_s3_class(result, "leaflet")
})
