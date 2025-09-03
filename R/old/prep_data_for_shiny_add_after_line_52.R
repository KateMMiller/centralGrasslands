#library(raster)
library(sf)
library(tidyverse)
library(terra)

nps_im <- st_read("./data_final/GIS/NPS_units_in_CGI_20250827_wgs84.shp")
cgr_ras <- terra::rast("./data_final/GIS/CGR_GAM_V2_WGS84_10km.tif")
cgr_ras_wgs <- terra::project(cgr_ras, "EPSG:4326")

cgr_shp <- terra::as.polygons(cgr_ras, round = T, aggregate = T, values = T, na.rm = T)

terra::writeVector(cgr_shp, "./data_final/GIS/CGR_GAM_V2_WGS84_10km.shp")
plot(cgr_shp[,2])

cgr_ras <- terra::rast("./data_final/GIS/CGR_GAM_V2_WGS84.tif")
cgr_nps <- terra::mask(cgr_ras, nps_im)
terra::writeRaster(cgr_nps, "./data_final/GIS/CGR_GAM_V2_WGS84_parks_terra.tif")
plot(cgr_ras)
