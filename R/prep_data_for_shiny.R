#-------------------------------------------------------
# prepping code for R Shiny and use with leaflet (wgs84)
#-------------------------------------------------------
library(raster)
library(sf)
library(tidyverse)

# #-- Converting to WGS84, so works with leaflet. Only have to run once, so commenting out after complete.
nps_im <- st_read("./data/GIS/CGI_parks_network.shp")
nps_im_wgs <- st_transform(nps_im, 4326)
st_crs(nps_im_wgs)
nps_im_wgs_cent <- st_centroid(nps_im_wgs)
nps_cent_latlong <- data.frame(st_drop_geometry(nps_im_wgs_cent),
                               long = st_coordinates(nps_im_wgs_cent)[,1],
                               lat = st_coordinates(nps_im_wgs_cent)[,2]
                               )
nps_im_wgs2 <- left_join(nps_im_wgs, nps_cent_latlong[,c("UNIT_CODE", "long", "lat")],
                       by = c("UNIT_CODE"))

st_write(nps_im_wgs2, "./data/GIS/CGI_parks_network_wgs.shp", append = FALSE)

# Convert CGR_map to shapefile for faster mapping
cgr_ras <- stars::read_stars("./data/GIS/CGR_GAM_V2_WGS84.tif")
cgr_shp <- st_as_sf(cgr_ras)
st_write(cgr_shp, "./data/GIS/CGR_GAM_V2_WGS84.shp")

# nps_im_1km <- st_read("./data/GIS/CGI_parks_network_1km.shp")
# nps_im_1km_wgs <- st_transform(nps_im_1km, 4326)
# st_crs(nps_im_1km_wgs)
# st_write(nps_im_1km_wgs, "./data/GIS/CGI_parks_network_1km_wgs.shp", append = FALSE)
#
# nps_im_10km <- st_read("./data/GIS/CGI_parks_network_10km.shp")
# nps_im_10km_wgs <- st_transform(nps_im_10km, 4326)
# st_crs(nps_im_10km_wgs)
# st_write(nps_im_10km_wgs, "./data/GIS/CGI_parks_network_10km_wgs.shp", append = FALSE)
#
# cgr <- terra::rast("./data/GIS/CGR_GAM_V2.tif")
# cgr_wgs <- terra::project(cgr, crs(nps_im_wgs), threads = 20)
# terra::writeRaster(cgr_wgs, "./data/GIS/CGR_GAM_V2_WGS84.tif")

# Load WGS 84 version of datasets
nps_im <- st_read("./data/GIS/CGI_parks_network_wgs.shp")
nps_im_1km <- st_read("./data/GIS/CGI_parks_network_1km_wgs.shp")
nps_im_10km <- st_read("./data/GIS/CGI_parks_network_10km_wgs.shp")
cgr_ras <- raster::raster("./data/GIS/CGR_GAM_V2_WGS84.tif")

park_prop_hab <- read.csv("./data/CGR_parks_prop_habitat.csv")

park_prop_hab_1km <- read.csv("./data/CGR_parks_prop_habitat_1km.csv")
park_prop_hab_10km <- read.csv("./data/CGR_parks_prop_habitat_10km.csv")

park_prop_comb1 <- left_join(park_prop_hab |> dplyr::select(UNIT_CODE, NETCODE, acres, Habitat, prop_hab, acres_hab),
                             park_prop_hab_1km |> dplyr::select(UNIT_CODE, NETCODE, acres, Habitat,
                                                         prop_hab_1km = prop_hab, acres_hab_1km = acres_hab),
                             by = c("UNIT_CODE", "NETCODE", "acres", "Habitat"))

park_prop_comb <- left_join(park_prop_comb1,
                            park_prop_hab_10km |> dplyr::select(UNIT_CODE, NETCODE, acres, Habitat,
                                                                prop_hab_10km = prop_hab, acres_hab_10km = acres_hab),
                            by = c("UNIT_CODE", "NETCODE", "acres", "Habitat"))

write.csv(park_prop_comb, "./data/GIS/CGR_parks_prop_habitat_all.csv")

#-- Visitation statistics --
# Data downloaded for all NPS units on 7/24/2024:
# https://irma.nps.gov/Stats/SSRSReports/National%20Reports/Query%20Builder%20for%20Public%20Use%20Statistics%20(1979%20-%20Last%20Calendar%20Year)

vis <- read.csv('./data/NPS_Public_Use_Statistics_2024.csv')
head(vis)
# total visitation
total_vis <- sum(vis$Recreation.Visits[-nrow(vis)], na.rm = T) #331,863,358
# cgr park visitation
cgr_vis <- sum(vis$Recreation.Visits[vis$CGI_Park == "X"], na.rm = T) #8,424,498
pct_vis <- (cgr_vis/total_vis)*100 #2.5 % of annual visitation

#-- CGR by land --
nps <- st_read("./data/GIS/NPS_boundary.shp") |> st_transform(5070)
st_crs(nps)
nps$area_m2 <- st_area(nps)
nps$acres <- nps$area_m2/4046.863
total_nps_acres <- sum(nps$acres, na.rm = T) #85,244,651 total acres in NPS lands

nps_im <- st_read("./data/GIS/CGI_parks_network.shp")
nps_im$area_m2 <- st_area(nps_im)
nps_im$acres <- nps_im$area_m2/4046.863
total_nps_acres_cgi <- sum(nps_im$acres, na.rm = T) #599,933.7 total acres in NPS lands
pct_nps_lands <- (total_nps_acres_cgi/total_nps_acres)*100 # = 0.7%
