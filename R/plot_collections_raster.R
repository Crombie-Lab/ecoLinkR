#' Create interactive map of climate rasters and collection sites
#'
#' Visualizes climate raster data (temperature, precipitation) overlaid with
#' collection site locations on an interactive leaflet map. Supports multiple
#' raster layers with individual legends and toggle controls.
#'
#' This function does NOT download rasters (already loaded into memory).
#' The rasters should be loaded separately using `terra::rast()`.
#'
#' @param raster_list List or named list of rasters (from `terra::rast()`).
#'   Can be a single raster or multiple rasters with names like `list(tmean = r1, ppt = r2)`.
#' @param collection_df Data frame with collection site locations and attributes.
#' @param lat_col Character. Name of latitude column (default: `"collection_latitude"`
#'   or `"c_latitude"`).
#' @param lon_col Character. Name of longitude column (default: `"collection_longitude"`
#'   or `"c_longitude"`).
#' @param label_col Character. Name of site label column for popup display
#'   (default: `"c_label"`).
#' @param palette Character. Color palette for rasters: `"viridis"` (default),
#'   `"magma"`, `"plasma"`, `"inferno"`, or other leaflet::colorNumeric palettes.
#' @param opacity Numeric. Raster layer opacity, 0-1 (default: `0.6`).
#' @param species_col Character. Optional column with species information for
#'   marker coloring. If NULL, all markers are the same color.
#' @param species_colors Named character vector. Colors for each species.
#'   Example: `c(Ce = "blue", Cb = "orange", Om = "red")`.
#'   If species_col is used but colors not provided, default palette is used.
#' @param proliferating_col Character. Optional column name indicating whether
#'   worms were proliferating (1 = yes, 0/NA = no). When supplied, marker colour
#'   is determined automatically per collection label:
#'   - `"red"` — proliferating AND species ID confirmed
#'   - `"lightgray"` — proliferating but species unknown
#'   - `"black"` — no organism detected
#'   Overrides `species_col`/`species_colors` colouring.
#' @param lulc Optional land cover layer. Can be:
#'   - `NULL` (default) - no land cover layer added.
#'   - A `SpatRaster` of land cover values (pre-processed, e.g. ESA WorldCover).
#'   - A single file path (character scalar) to a GeoTIFF produced by
#'     [fetch_climate()] with `lulc_tiles` specified.
#'   - A character vector of ESA WorldCover tile URLs to download on the fly.
#'     Tiles are cached in `tempdir()` and cropped to the sites bounding box.
#' @param lulc_opacity Numeric. Opacity for the land cover layer (default: `0.65`).
#' @param show_lulc Logical. Whether the land cover layer is visible by default
#'   in the layer control (default: `TRUE`).
#' @param show_rasters Character vector. Which rasters to show by default
#'   (others hidden). Default: first raster only. Set to `NULL` to show all.
#' @param out_html Character. Optional path to save the map as an HTML file.
#'   If `NULL`, map is displayed but not saved.
#' @param title Character. Map title displayed in the upper left.
#'   Default: `"Climate & Collection Sites"`.
#' @param max_bytes Numeric. Maximum bytes allowed for raster image rendering in
#'   leaflet. Higher values produce sharper maps at the cost of file size.
#'   Default: `16 * 1024^2` (16 MB). The leaflet package default is 4 MB.
#'
#' @return A leaflet map object (invisibly). If `out_html` is provided,
#'   the map is also saved to that HTML file.
#'
#' @details
#' The function automatically:
#' - Reprojects rasters to EPSG:4326 (WGS84) if needed
#' - Creates appropriate color scales for each raster
#' - Builds popups with collection site information
#' - Fits map bounds to data extent
#' - Adds layer controls for toggling overlays on/off
#'
#' Raster layers can be toggled independently using the Layers Control panel.
#' Collection site markers show popups on click with site metadata.
#'
#' @examples
#' \dontrun{
#' # Load rasters
#' r_tmean <- terra::rast("data/processed/prism_tmean.tif")
#' r_ppt <- terra::rast("data/processed/prism_ppt.tif")
#'
#' # Create map with default settings
#' plot_collections_raster(
#'   raster_list = list(Temperature = r_tmean, Precipitation = r_ppt),
#'   collection_df = my_sites,
#'   show_rasters = "Temperature"
#' )
#'
#' # Create map with species coloring and save
#' plot_collections_raster(
#'   raster_list = list(r_tmean, r_ppt),
#'   collection_df = my_sites,
#'   species_col = "species_id",
#'   species_colors = c(Ce = "blue", Cb = "orange", Om = "red"),
#'   out_html = "plots/climate_map.html"
#' )
#' }
#'
#' @export
plot_collections_raster <- function(
    raster_list,
    collection_df,
    lat_col = NULL,
    lon_col = NULL,
    label_col = "c_label",
    palette = "viridis",
    opacity = 0.6,
    species_col = NULL,
    species_colors = NULL,
    proliferating_col = NULL,
    lulc = NULL,
    lulc_opacity = 0.65,
    show_lulc = TRUE,
    show_rasters = NULL,
    out_html = NULL,
    title = "Climate & Collection Sites",
    max_bytes = 16 * 1024^2
) {

  # ---- validation ----
  if (!inherits(raster_list, "list")) {
    # Single raster - convert to list
    raster_list <- list(raster_list)
  }

  if (length(raster_list) == 0) {
    stop("raster_list must contain at least one raster")
  }

  # Check all inputs are SpatRasters
  for (i in seq_along(raster_list)) {
    if (!inherits(raster_list[[i]], "SpatRaster")) {
      stop("Element ", i, " of raster_list is not a SpatRaster (from terra::rast())")
    }
  }

  # Auto-name rasters if not named
  if (is.null(names(raster_list)) || any(names(raster_list) == "")) {
    names(raster_list) <- paste0("Layer_", seq_along(raster_list))
  }

  # ---- prepare sites ----
  if (!is.data.frame(collection_df)) {
    stop("collection_df must be a data frame")
  }

  # Auto-detect lat/lon columns
  if (is.null(lat_col)) {
    lat_candidates <- c("collection_latitude", "c_latitude", "GPSLatitude", "lat", "latitude")
    lat_col <- intersect(lat_candidates, names(collection_df))[1]
    if (is.na(lat_col)) {
      stop("Could not auto-detect latitude column. Specify 'lat_col' explicitly.")
    }
  }

  if (is.null(lon_col)) {
    lon_candidates <- c("collection_longitude", "c_longitude", "GPSLongitude", "lon", "longitude")
    lon_col <- intersect(lon_candidates, names(collection_df))[1]
    if (is.na(lon_col)) {
      stop("Could not auto-detect longitude column. Specify 'lon_col' explicitly.")
    }
  }

  if (!lat_col %in% names(collection_df) || !lon_col %in% names(collection_df)) {
    stop("Latitude/longitude columns not found in collection_df")
  }

  # Filter to valid coordinates
  sites <- collection_df[!is.na(collection_df[[lat_col]]) & !is.na(collection_df[[lon_col]]), ]

  if (nrow(sites) == 0) {
    stop("No valid lat/lon coordinates found in collection_df")
  }

  # ---- check/reproject rasters to WGS84 ----
  for (i in seq_along(raster_list)) {
    r_crs <- terra::crs(raster_list[[i]])
    if (is.na(r_crs) || r_crs == "") {
      message("Raster '", names(raster_list)[i], "' has no CRS. Assuming EPSG:4326.")
      terra::crs(raster_list[[i]]) <- "EPSG:4326"
    } else if (!grepl("4326", r_crs, ignore.case = TRUE)) {
      message("Reprojecting raster '", names(raster_list)[i], "' to EPSG:4326...")
      raster_list[[i]] <- terra::project(raster_list[[i]], "EPSG:4326")
    }
  }

  # ---- build color palettes ----
  palettes <- list()
  for (i in seq_along(raster_list)) {
    r <- raster_list[[i]]
    mm <- terra::minmax(r)
    r_min <- mm[1, 1]
    r_max <- mm[2, 1]

    if (!is.finite(r_min) || !is.finite(r_max)) {
      warning("Raster '", names(raster_list)[i], "' has no finite values")
      palettes[[i]] <- leaflet::colorNumeric(
        palette = palette,
        domain = c(0, 1),
        na.color = "transparent"
      )
    } else {
      palettes[[i]] <- leaflet::colorNumeric(
        palette = palette,
        domain = c(r_min, r_max),
        na.color = "transparent"
      )
    }
  }
  names(palettes) <- names(raster_list)

  # ---- prepare land cover raster ----
  esa_classes <- data.frame(
    value = c(10,   20,          30,           40,        50,
              60,               70,            80,
              90,               95,            100),
    label = c("Tree Cover", "Shrubland", "Grassland", "Cropland", "Built-up",
              "Bare / Sparse Veg", "Snow and Ice", "Permanent Water",
              "Herbaceous Wetland", "Mangroves", "Moss and Lichen"),
    color = c("#006400", "#FFBB22", "#FFFF4C", "#F096FF", "#FA0000",
              "#B4B4B4", "#F0F0F0", "#0064C8",
              "#0096A0", "#00CF75", "#FAE6A0"),
    stringsAsFactors = FALSE
  )
  lulc_raster <- NULL
  lulc_pal_df <- NULL
  pal_lulc    <- NULL

  if (!is.null(lulc)) {
    if (inherits(lulc, "SpatRaster")) {
      lulc_raster <- lulc
      if (!grepl("4326", terra::crs(lulc_raster), ignore.case = TRUE)) {
        lulc_raster <- terra::project(lulc_raster, "EPSG:4326")
      }
    } else if (is.character(lulc) && length(lulc) == 1 && file.exists(lulc)) {
      # Pre-processed GeoTIFF (e.g. from fetch_climate with lulc_tiles)
      lulc_raster <- terra::rast(lulc)
      if (!grepl("4326", terra::crs(lulc_raster), ignore.case = TRUE)) {
        lulc_raster <- terra::project(lulc_raster, "EPSG:4326")
      }
    } else if (is.character(lulc)) {
      tmp_dir    <- tempdir()
      tile_files <- character(0)
      for (i in seq_along(lulc)) {
        fname <- file.path(tmp_dir, basename(lulc[i]))
        if (!file.exists(fname)) {
          message(sprintf("LULC tile %d/%d: downloading...", i, length(lulc)))
          tryCatch(
            utils::download.file(lulc[i], fname, mode = "wb", quiet = TRUE),
            error = function(e) message("  WARNING - Failed: ", e$message)
          )
        } else {
          message(sprintf("LULC tile %d/%d: cached", i, length(lulc)))
        }
        if (file.exists(fname)) tile_files <- c(tile_files, fname)
      }
      if (length(tile_files) == 0) {
        warning("No LULC tiles could be downloaded; land cover layer skipped.")
      } else {
        lng_range <- range(sites[[lon_col]], na.rm = TRUE)
        lat_range <- range(sites[[lat_col]], na.rm = TRUE)
        pad       <- 0.5
        sites_ext <- terra::ext(
          lng_range[1] - pad, lng_range[2] + pad,
          lat_range[1] - pad, lat_range[2] + pad
        )
        tile_rasts <- lapply(tile_files, function(f) {
          r <- terra::rast(f)
          tryCatch(terra::crop(r, sites_ext), error = function(e) {
            warning("Could not crop LULC tile: ", basename(f)); NULL
          })
        })
        tile_rasts <- Filter(Negate(is.null), tile_rasts)
        if (length(tile_rasts) > 0) {
          lulc_raster <- if (length(tile_rasts) == 1) tile_rasts[[1]] else
            do.call(terra::mosaic, c(tile_rasts, list(fun = "first")))
          lulc_raster <- terra::aggregate(lulc_raster, fact = 30, fun = "modal")
          lulc_raster <- terra::project(lulc_raster, "EPSG:4326")
        } else {
          warning("No LULC tiles overlapped the sites extent; land cover layer skipped.")
        }
      }
    } else {
      stop("'lulc' must be a SpatRaster or a character vector of tile URLs.")
    }

    if (!is.null(lulc_raster)) {
      present_vals <- sort(unique(terra::values(lulc_raster, na.rm = TRUE)))
      lulc_pal_df  <- esa_classes[esa_classes$value %in% present_vals, ]
      pal_lulc     <- leaflet::colorFactor(
        palette  = lulc_pal_df$color,
        levels   = lulc_pal_df$value,
        na.color = "transparent"
      )
    }
  }

  # ---- prepare site markers ----
  if (!is.null(proliferating_col) && proliferating_col %in% names(sites)) {
    # Color per c_label: red = species confirmed + proliferating,
    # lightgray = proliferating but unknown species, black = no organism
    has_species_col <- !is.null(species_col) && species_col %in% names(sites)
    if (has_species_col) {
      sites <- sites %>%
        dplyr::group_by(.data[[label_col]]) %>%
        dplyr::mutate(
          marker_color = dplyr::case_when(
            any(
              .data[[proliferating_col]] == 1 &
                !is.na(.data[[species_col]]) &
                .data[[species_col]] != "" &
                stringr::str_to_lower(.data[[species_col]]) != "n/a",
              na.rm = TRUE
            ) ~ "red",
            any(.data[[proliferating_col]] == 1, na.rm = TRUE) ~ "lightgray",
            TRUE ~ "black"
          )
        ) %>%
        dplyr::ungroup()
    } else {
      sites <- sites %>%
        dplyr::group_by(.data[[label_col]]) %>%
        dplyr::mutate(
          marker_color = dplyr::case_when(
            any(.data[[proliferating_col]] == 1, na.rm = TRUE) ~ "lightgray",
            TRUE ~ "black"
          )
        ) %>%
        dplyr::ungroup()
    }
    use_awesome <- TRUE
  } else if (!is.null(species_col) && species_col %in% names(sites)) {
    # Color by species
    species <- sites[[species_col]]
    if (is.null(species_colors)) {
      unique_species <- unique(species[!is.na(species)])
      species_colors <- c(
        Ce = "blue", Cb = "orange", Om = "red", N2 = "purple",
        C = "green"
      )[unique_species]
      if (any(is.na(species_colors))) {
        missing <- unique_species[is.na(species_colors[unique_species])]
        extra_colors <- grDevices::rainbow(length(missing))
        species_colors <- c(species_colors, stats::setNames(extra_colors, missing))
      }
    }
    sites$marker_color <- species_colors[as.character(species)]
    sites$marker_color[is.na(sites$marker_color)] <- "gray"
    use_awesome <- FALSE
  } else {
    sites$marker_color <- "blue"
    use_awesome <- FALSE
  }

  # Build popups
  popup_cols <- c(label_col, lat_col, lon_col)
  popup_cols <- popup_cols[popup_cols %in% names(sites)]

  # Build popup HTML strings
  site_popups <- paste0(
    "<h3>", sites[[label_col]], "</h3>",
    "<hr/>",
    "<strong>Latitude:</strong> ", format(round(sites[[lat_col]], 6), nsmall = 6), "<br/>",
    "<strong>Longitude:</strong> ", format(round(sites[[lon_col]], 6), nsmall = 6)
  )

  # Add species info if available
  if (!is.null(species_col) && species_col %in% names(sites)) {
    site_popups <- paste0(
      site_popups,
      "<br/><strong>Species:</strong> ",
      sites[[species_col]]
    )
  }

  # ---- set default shown rasters ----
  if (is.null(show_rasters)) {
    show_rasters <- names(raster_list)  # Show all
  } else if (length(show_rasters) == 1 && !show_rasters %in% names(raster_list)) {
    # Use first in list
    show_rasters <- names(raster_list)[1]
  }

  # ---- build base map ----
  m <- leaflet::leaflet(
    options = leaflet::leafletOptions(zoomControl = TRUE)
  ) %>%
    leaflet::addTiles(group = "OSM (Base)") %>%
    leaflet::addControl(
      html = paste0("<h4>", title, "</h4>"),
      position = "topleft"
    )

  # ---- add land cover layer (below climate rasters) ----
  if (!is.null(lulc_raster) && !is.null(pal_lulc)) {
    m <- m %>%
      leaflet::addRasterImage(
        lulc_raster,
        colors   = pal_lulc,
        opacity  = lulc_opacity,
        group    = "Land Cover (ESA)",
        maxBytes = max_bytes
      )
  }

  # ---- add raster layers ----
  for (i in seq_along(raster_list)) {
    r <- raster_list[[i]]
    layer_name <- names(raster_list)[i]
    pal <- palettes[[layer_name]]

    m <- m %>%
      leaflet::addRasterImage(
        r,
        colors = pal,
        opacity = opacity,
        group = layer_name,
        maxBytes = max_bytes
      )
  }

  # ---- add collection site markers ----
  if (isTRUE(use_awesome)) {
    site_icons <- leaflet::awesomeIcons(
      icon        = "circle",
      library     = "fa",
      markerColor = sites$marker_color,
      iconColor   = "white"
    )
    m <- m %>%
      leaflet::addAwesomeMarkers(
        data         = sites,
        lng          = sites[[lon_col]],
        lat          = sites[[lat_col]],
        icon         = site_icons,
        popup        = site_popups,
        popupOptions = leaflet::popupOptions(maxWidth = 500),
        group        = "Collection Sites"
      )
  } else {
    m <- m %>%
      leaflet::addCircleMarkers(
        data        = sites,
        lng         = sites[[lon_col]],
        lat         = sites[[lat_col]],
        radius      = 5,
        stroke      = TRUE,
        weight      = 2,
        color       = "black",
        fillColor   = ~marker_color,
        fillOpacity = 0.8,
        popup       = site_popups,
        group       = "Collection Sites"
      )
  }

  # ---- add land cover legend ----
  if (!is.null(lulc_raster) && !is.null(lulc_pal_df)) {
    m <- m %>%
      leaflet::addLegend(
        position = "bottomleft",
        colors   = lulc_pal_df$color,
        labels   = lulc_pal_df$label,
        title    = "Land Cover (ESA 2021)",
        opacity  = 0.9,
        layerId  = "legend_lulc"
      )
  }

  # ---- add site colour legend ----
  if (isTRUE(use_awesome)) {
    m <- m %>%
      leaflet::addLegend(
        position = "bottomleft",
        colors   = c("red", "lightgray", "black"),
        labels   = c("Species ID confirmed", "Proliferating (unknown species)", "No organism"),
        title    = "Collection Sites",
        opacity  = 0.9
      )
  }

  # ---- add legends ----
  # Add legend for each raster
  for (i in seq_along(raster_list)) {
    r <- raster_list[[i]]
    layer_name <- names(raster_list)[i]
    pal <- palettes[[layer_name]]
    mm <- terra::minmax(r)
    r_min <- mm[1, 1]
    r_max <- mm[2, 1]

    m <- m %>%
      leaflet::addLegend(
        pal = pal,
        values = c(r_min, r_max),
        title = layer_name,
        opacity = 0.9,
        position = "bottomright",
        layerId = paste0("legend_", i)
      )
  }

  # ---- add layer controls ----
  overlay_groups <- c(
    if (!is.null(lulc_raster)) "Land Cover (ESA)" else NULL,
    names(raster_list),
    "Collection Sites"
  )

  m <- m %>%
    leaflet::addLayersControl(
      baseGroups = "OSM (Base)",
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  # ---- hide non-default rasters ----
  hidden_rasters <- setdiff(names(raster_list), show_rasters)
  for (hidden in hidden_rasters) {
    m <- m %>%
      leaflet::hideGroup(hidden)
  }

  if (!is.null(lulc_raster) && !isTRUE(show_lulc)) {
    m <- m %>% leaflet::hideGroup("Land Cover (ESA)")
  }

  # ---- fit bounds to data ----
  if (nrow(sites) > 0) {
    lng_range <- range(sites[[lon_col]], na.rm = TRUE)
    lat_range <- range(sites[[lat_col]], na.rm = TRUE)

    # Add some padding
    lng_pad <- (lng_range[2] - lng_range[1]) * 0.1
    lat_pad <- (lat_range[2] - lat_range[1]) * 0.1

    m <- m %>%
      leaflet::fitBounds(
        lng_range[1] - lng_pad, lat_range[1] - lat_pad,
        lng_range[2] + lng_pad, lat_range[2] + lat_pad
      )
  }

  # ---- save HTML if requested ----
  if (!is.null(out_html)) {
    out_dir <- dirname(out_html)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    htmlwidgets::saveWidget(m, out_html, selfcontained = FALSE)
    message("Map saved to: ", out_html)
  }

  invisible(m)
}
