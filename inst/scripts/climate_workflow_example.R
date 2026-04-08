# Climate Data Integration Workflow for ecolinkR
#
# This script demonstrates the complete workflow for:
# 1. Downloading/accessing climate raster data
# 2. Extracting climate values at collection sites
# 3. Joining climate data to collection data
# 4. Visualizing climate and collection data on an interactive map
#
# Last updated: 2026-04-02

library(ecolinkR)
library(dplyr)
library(terra)

# ============================================================
# STEP 1: Obtain climate raster data
# ============================================================

# Option A: Point to existing raster files in a directory
clim_data <- download_climate_data(
  variable = c("tmean", "ppt"),
  raster_dir = "data/processed",  # <- Change to your raster directory
  use_prism = FALSE
)

# Option B: Download from PRISM (requires prism package)
# clim_data <- download_climate_data(
#   variable = c("tmean", "ppt"),
#   start_date = "2024-11-01",
#   end_date = "2025-02-01",
#   raster_dir = "data/climate",
#   use_prism = TRUE
# )

str(clim_data)

# ============================================================
# STEP 2: Load rasters into memory
# ============================================================

r_tmean <- terra::rast(clim_data$tmean)
r_ppt <- terra::rast(clim_data$ppt)

# Check raster metadata
terra::crs(r_tmean)
terra::ext(r_tmean)
terra::nlyr(r_tmean)

# ============================================================
# STEP 3: Load your collection data
# ============================================================

# Load from CSV (example)
collection_data <- readr::read_csv("data/processed/collection_with_gps.csv")

# Check for required columns
head(collection_data[, c("c_label", "GPSLatitude", "GPSLongitude")])

# ============================================================
# STEP 4: Extract climate values at collection sites
# ============================================================

climate_extracted <- extract_climate_values(
  raster_paths = c(clim_data$tmean, clim_data$ppt),
  collection_df = collection_data,
  lat_col = "GPSLatitude",
  lon_col = "GPSLongitude",
  label_col = "c_label",
  var_names = c("mean_temp_celsius", "mean_precip_mm"),
  buffer_distance = NULL,  # Set to e.g. 10000 for 10km buffer
  out_csv = "data/processed/extracted_climate_values.csv"
)

head(climate_extracted)

# ============================================================
# STEP 5: Join climate data to collection data
# ============================================================

collection_with_climate <- join_climate_to_collection(
  collection_df = collection_data,
  climate_df = climate_extracted,
  by = "c_label",
  join_type = "left",
  out_csv = "data/processed/collection_with_climate_data.csv",
  verbose = TRUE
)

# Check result
head(collection_with_climate[, c("c_label", "species_id", "mean_temp_celsius", "mean_precip_mm")])

# ============================================================
# STEP 6: Visualize climate and collection sites on map
# ============================================================

map_climate_raster(
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
  out_html = "plots/climate_collection_map.html",
  title = "Collection Sites & Climate Data"
)

message("Map saved to: plots/climate_collection_map.html")

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

ggsave("plots/temp_vs_precip.pdf", width = 8, height = 6)
