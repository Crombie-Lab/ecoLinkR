library(devtools)
load_all()

check()

rename_files("download_climate_data", "fetch_climate")
use_test("fetch_climate")
test()

rename_files("extract_climate_values", "extract_climate")
use_test("extract_climate")
test()

rename_files("join_climate_to_collection", "join_climate")  
use_test("join_climate")
test()

rename_files("map_climate_raster","plot_collections_raster")
use_test("plot_collections_raster")
test()

library(devtools)
load_all()
document()
