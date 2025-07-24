#-------------------------------------------------------------------
# Global file loads objects available to server and ui
#-------------------------------------------------------------------
library(sf)
library(raster)
library(leaflet)

# WGS 84 version of datasets
nps_im <- st_read("../data/GIS/CGI_parks_network_wgs.shp")
#nps_bbox1 <- st_bbox(nps_im) * 1.01 # added 1% buffer
nps_bbox <- data.frame(xmin = -113.16628, ymin = 29.38215,
                       xmax = -95.35811, ymax = 47.85992)

# long_mean = mean(nps_bbox$xmin, nps_bbox$xmax)
# lat_mean = mean(nps_bbox$ymin, nps_bbox$ymax)

#cent1 <- st_coordinates(st_centroid(st_as_sfc(nps_bbox)))
cent <- data.frame(long = -104.2622, lat = 38.62103)

nps_im_df <- data.frame(st_drop_geometry(nps_im))

nps_im_1km <- st_read("../data/GIS/CGI_parks_network_1km_wgs.shp")
nps_im_10km <- st_read("../data/GIS/CGI_parks_network_10km_wgs.shp")
#cgr_ras <- raster::raster("../data/GIS/CGR_GAM_V2_WGS84.tif")
cgr_shp <- st_read("../data/GIS/CGR_GAM_V2_WGS84.shp")
park_prop_hab <- read.csv("../data/GIS/CGR_parks_prop_habitat_all.csv")

network_list <- sort(unique(nps_im$NETCODE))
park_list <- sort(unique(nps_im$UNIT_CODE))

