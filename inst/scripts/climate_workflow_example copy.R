# Climate Data Integration Workflow for ecolinkR
#
# This script demonstrates the complete workflow for:
# 1. Downloading/accessing climate raster data
# 2. Extracting climate values at collection sites
# 3. Joining climate data to collection data
# 4. Visualizing climate and collection data on an interactive map
#
# Last updated: 2026-04-15 (fixed for correct value extraction)
library(devtools)
load_all()

library(dplyr)
library(terra)
library(prism)

# ============================================================
# STEP 1: Obtain climate raster data
# ============================================================

# Download PRISM data for date range, crop to ROI, calculate mean
# Note: PRISM data has a processing lag; use dates at least 1-2 weeks old
message("Starting climate data fetch...")
message("Date range: 2024-11-01 to 2025-02-01")
message("Region: lat [27.95, 28.10], lon [-80.67, -80.55]")

# Start with just tmean to test
clim_data <- fetch_climate(
  variable = c("tmean", "ppt"),
  start_date = "2024-11-01",
  end_date = "2025-02-01",
  lat_min = 27.95,
  lat_max = 28.10,
  lon_min = -80.67,
  lon_max = -80.55,
  out_dir = "/Users/lainieboldus/Desktop/Crombie Lab/climate_test/data/processed",
  resolution = "800m",
  overwrite = TRUE,
  out_list = TRUE
)

# Check that rasters were created
message("Result:")
print(clim_data)

if (is.null(clim_data$tmean) || is.null(clim_data$ppt)) {
  stop("Failed to create climate rasters. Check the error messages above.")
}

message("Climate tiles created successfully!")

# ============================================================
# STEP 2: Load rasters into memory
# ============================================================

if (!file.exists(clim_data$tmean) || !file.exists(clim_data$ppt)) {
  stop("One or more raster files do not exist. Check paths: ", 
       clim_data$tmean, " and ", clim_data$ppt)
}

r_tmean <- terra::rast(clim_data$tmean)
r_ppt <- terra::rast(clim_data$ppt)

# Check raster metadata
print(terra::crs(r_tmean))
print(terra::ext(r_tmean))
print(terra::nlyr(r_tmean))

# ============================================================
# STEP 3: Load your collection data
# ============================================================

collection_data <- readr::read_csv("/Users/lainieboldus/Desktop/Crombie Lab/wnc_data25:26/full6collections.csv")

# Check for required columns
stopifnot(all(c("c_label", "GPSLatitude", "GPSLongitude") %in% names(collection_data)))

# ============================================================
# STEP 4: Extract climate values at collection sites
# ============================================================

climate_extracted <- ecoLinkR::extract_climate(
  raster_paths = c(clim_data$tmean, clim_data$ppt),
  collection_df = collection_data,
  lat_col = "GPSLatitude",
  lon_col = "GPSLongitude",
  label_col = "c_label",
  var_names = c("mean_temp_celsius", "mean_precip_mm"),
  buffer_distance = NULL,  # Set to e.g. 10000 for 10km buffer
  out_csv = "/Users/lainieboldus/Desktop/Crombie Lab/climate_test/data/processed/extracted_climate_values.csv"
)

print(head(climate_extracted))

# ============================================================
# STEP 5: Join climate data to collection data
# ============================================================

collection_with_climate <- ecoLinkR::join_climate(
  collection_df = collection_data,
  climate_df = climate_extracted,
  by = "c_label",
  join_type = "left",
  out_csv = "/Users/lainieboldus/Desktop/Crombie Lab/climate_test/data/processed/collection_with_climate_data.csv",
  verbose = TRUE
)

print(head(collection_with_climate[, c("c_label", "species_id", "mean_temp_celsius", "mean_precip_mm")]))

# ============================================================
# STEP 6: Visualize climate and collection sites on map
# ============================================================

ecoLinkR::plot_collections_raster(
  raster_list = list(Temperature_C = r_tmean, Precipitation_mm = r_ppt),
  collection_df = collection_data,
  lat_col = "GPSLatitude",
  lon_col = "GPSLongitude",
  label_col = "c_label",
  palette = "viridis",
  opacity = 0.6,
  species_col = "species_id",
  species_colors = c(Ce = "blue", Cb = "orange", Om = "red", N2 = "purple"),
  show_rasters = "Temperature_C",  # Show temperature by default
  out_html = "/Users/lainieboldus/Desktop/Crombie Lab/climate_test/plots/climate_collection_map.html",
  title = "Collection Sites & Climate Data"
)

message("Map saved to: /Users/lainieboldus/Desktop/Crombie Lab/climate_test/plots/climate_collection_map.html")

# ============================================================
# OPTIONAL: Further analysis with combined data
# ============================================================

# Summary statistics by species
summary_by_species <- collection_with_climate %>%
  group_by(species_id) %>%
  summarise(
    n_collections = n(),
    mean_temp = mean(mean_temp_celsius, na.rm = TRUE),
    sd_temp = sd(mean_temp_celsius, na.rm = TRUE),
    mean_precip = mean(mean_precip_mm, na.rm = TRUE),
    sd_precip = sd(mean_precip_mm, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_by_species)

# Create visualizations
library(ggplot2)

# Temperature vs Precipitation
ggplot(collection_with_climate, aes(x = mean_temp_celsius, y = mean_precip_mm, color = species_id)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Collection Sites: Temperature vs Precipitation",
    x = "Mean Temperature (°C)",
    y = "Mean Precipitation (mm/day)",
    color = "Species"
  )

ggsave("/Users/lainieboldus/Desktop/Crombie Lab/climate_test/plots/temp_vs_precip.pdf", width = 8, height = 6)
