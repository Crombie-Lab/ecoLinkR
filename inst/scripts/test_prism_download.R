# Diagnostic script to test PRISM download functionality
library(prism)
library(terra)

# Setup
test_dir <- "/tmp/prism_test_diagnostic"
dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
prism_set_dl_dir(test_dir)

message("Testing PRISM download...")
message("Download directory: ", test_dir)

# Try downloading a small date range
start_date <- as.Date("2024-11-01")
end_date <- as.Date("2024-11-05")  # Just 5 days for speed

message("\nAttempting to download tmean from ", start_date, " to ", end_date, "...")

tryCatch({
  get_prism_dailys(
    type = "tmean",
    minDate = start_date,
    maxDate = end_date,
    keepZip = FALSE,
    resolution = "4km"
  )
  message("Download completed successfully!")
}, error = function(e) {
  message("Download failed with error: ", e$message)
})

# Check what was actually downloaded
message("\n--- Files in download directory ---")
all_files <- list.files(test_dir, recursive = TRUE, full.names = TRUE)
if (length(all_files) == 0) {
  message("NO FILES FOUND in download directory!")
} else {
  message("Found ", length(all_files), " files:")
  for (f in head(all_files, 20)) {
    message("  ", basename(dirname(f)), "/", basename(f))
  }
}

# Check for .bil files specifically
message("\n--- Searching for .bil files ---")
bil_files <- list.files(test_dir, pattern = "\\.bil$", recursive = TRUE, full.names = TRUE)
if (length(bil_files) == 0) {
  message("NO .bil FILES FOUND!")
  message("Looking for other raster formats...")
  other_rasters <- list.files(test_dir, pattern = "\\.(tif|TIF|img|grd)$", recursive = TRUE, full.names = TRUE)
  if (length(other_rasters) > 0) {
    message("Found these rasters instead:")
    for (f in other_rasters) message("  ", basename(f))
  }
} else {
  message("Found ", length(bil_files), " .bil files")
  for (f in head(bil_files, 5)) message("  ", basename(f))
}

message("\nDiagnostic complete!")
