
# code to download DEM
# for the area around the Red Butte Research Natural Area (RNA)
# in the Wasatch foothills above the University of Utah campus

# as per Edzer Pebesma's statement here:
# https://github.com/r-spatial/sf/issues/1341#issuecomment-734435367
# should save all spatial data to inst/extdata in their native format, rather than .rda

# but devtools::check() fails hard with this file

library(dplyr)
library(FedData)
library(sf)
library(terra)

red_butte_canyon <- read_sf("inst/extdata/red_butte_rna.geojson")

# download DEM
temp <- file.path(tempdir(), "FedData", "raw", "ned")

red_butte_dem <-
  FedData::get_ned(st_buffer(red_butte_canyon, 500),
                   label = "RBC", res = "1",
                   raw.dir = temp,
                   extraction.dir = temp)

red_butte_dem <-
  red_butte_dem %>%
  rast() %>%
  project(st_crs(red_butte_canyon)$proj4string) %>%
  crop(red_butte_canyon) %>%
  mask(., rasterize(vect(red_butte_canyon), .))


# visual check
plot(red_butte_dem)
red_butte_canyon %>%
  st_geometry() %>%
  plot(add = TRUE)

writeRaster(red_butte_dem,
            filename = "inst/extdata/red_butte_dem.tif",
            overwrite = TRUE)
