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

  # ---- prepare site markers ----
  # Color by species if specified
  if (!is.null(species_col) && species_col %in% names(sites)) {
    species <- sites[[species_col]]

    if (is.null(species_colors)) {
      # Default colors
      unique_species <- unique(species[!is.na(species)])
      species_colors <- c(
        Ce = "blue", Cb = "orange", Om = "red", N2 = "purple",
        C = "green"
      )[unique_species]
      if (any(is.na(species_colors))) {
        # Generate more colors if needed
        missing <- unique_species[is.na(species_colors[unique_species])]
        extra_colors <- grDevices::rainbow(length(missing))
        species_colors <- c(species_colors, stats::setNames(extra_colors, missing))
      }
    }

    sites$marker_color <- species_colors[as.character(species)]
    sites$marker_color[is.na(sites$marker_color)] <- "gray"
  } else {
    sites$marker_color <- "blue"
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
    leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "OSM (Base)") %>%
    leaflet::addControl(
      html = paste0("<h4>", title, "</h4>"),
      position = "topleft"
    )

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
  m <- m %>%
    leaflet::addCircleMarkers(
      data = sites,
      lng = sites[[lon_col]],
      lat = sites[[lat_col]],
      radius = 5,
      stroke = TRUE,
      weight = 2,
      color = "black",
      fillColor = ~marker_color,
      fillOpacity = 0.8,
      popup = site_popups,
      group = "Collection Sites"
    )

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
  overlay_groups <- c(names(raster_list), "Collection Sites")

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
