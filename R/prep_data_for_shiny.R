#-------------------------------------------------------
# prepping code for R Shiny and use with leaflet (wgs84)
#-------------------------------------------------------
library(raster)
library(sf)
library(tidyverse)
library(spatialEco)

sf_use_s2(FALSE)

# #-- Converting to WGS84, so works with leaflet. Only have to run once, so commenting out after complete.
nps_im <- st_read("./data/GIS/CGI_parks_network.shp")
nps_im_1km <- st_buffer(nps_im, 1000)
nps_im_10km <- st_buffer(nps_im, 10000)

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

nps_im_1km_wgs <- st_transform(nps_im_1km, 4326)
st_crs(nps_im_1km_wgs)
st_write(nps_im_1km_wgs, "./data/GIS/CGI_parks_network_1km_wgs.shp", append = FALSE)
#
nps_im_10km_wgs <- st_transform(nps_im_10km, 4326)
st_crs(nps_im_10km_wgs)
st_write(nps_im_10km_wgs, "./data/GIS/CGI_parks_network_10km_wgs.shp", append = FALSE)

# cgr <- terra::rast("./data/GIS/CGR_GAM_V2.tif")
# cgr_wgs <- terra::project(cgr, crs(nps_im_wgs), threads = 20)
# terra::writeRaster(cgr_wgs, "./data/GIS/CGR_GAM_V2_WGS84.tif")

# tried to make a map tile- didn't work
# library(raster)
# tile(file = "./data/GIS/CGR_GAM_V2_WGS84.tif",
#      tiles = "./data/GIS/",
#      zoom = "0:10",
#      crs = 4326)
#
# cgr_ras <- terra::rast("./data/GIS/CGR_GAM_V2_park10km_extract.tif")
# cgr_wgs <- terra::project(cgr_ras, crs(nps_im_wgs), threads = 20)
# terra::writeRaster(cgr_wgs, "./data/GIS/CGR_GAM_V2_WGS84_10km.tif")

cgr_border <- st_read("./data/GIS/Grasslands_Roadmap_boundary_Aug_2021.shp")
cgr_border_wgs <- st_transform(cgr_border, 4326)
st_write(cgr_border_wgs, "./data/GIS/Grasslands_Roadmap_boundary_Aug_2021_WGS84.shp")
# Load WGS 84 version of datasets
nps_im_wgs <- st_read("./data/GIS/CGI_parks_network_wgs.shp")
nps_im_1km_wgs <- st_read("./data/GIS/CGI_parks_network_1km_wgs.shp")
nps_im_10km_wgs <- st_read("./data/GIS/CGI_parks_network_10km_wgs.shp")

# Try compressing raster for smaller size/faster plotting?
# cgr_ras <- raster::raster("./data/GIS/CGR_GAM_V2_WGS84.tif")
# raster::writeRaster(cgr_ras, filename = "./data/GIS/CGR_GAM_V2_WGS84_compress.tif", format = "GTiff",
#                     options = c("COMPRESS=LZW"), datatype = "FLT4S", overwrite = T)
cgr_ras <- raster::raster("./data/GIS/CGR_GAM_V2_WGS84_compress.tif")

# I couldn't convert the raster CGR dataset to shapefile in R- too big.
# Instead, I did this in ArcPro for the 10km clip, then am dissolving
# to make smaller file, joining with nps units, then transforming to wgs84.

cgr_shp <- st_read("./data/GIS/CGR_GAM_V2_UTM_NAD83_10km.shp")
cgr_shp_diss <- sf_dissolve(cgr_shp, y = "gridcode")
cgr_shp_park <- st_join(cgr_shp_diss, nps_im_10km)
cgr_shp_park2 <- st_transform(cgr_shp_park, 4326)
st_write(cgr_shp_park2, "./data/GIS/CGR_GAM_V2_10km_WGS84_diss.shp", append = F)
#st_write(cgr_shp_park2, "./data/GIS/CGR_GAM_V2_10km_WGS84_park.shp", append = F)

cgr_shp <- st_read("./data/GIS/CGR_GAM_V2_UTM_NAD83_10km.shp") |> st_transform(4326)
st_write(cgr_shp, "./data/GIS/CGR_GAM_V2_WGS_10km.shp", append = F)
# ENDED HERE

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
cgr_vis <- sum(vis$Recreation.Visits[vis$CGI_Park == "X"], na.rm = T) #15,999,812
pct_vis <- (cgr_vis/total_vis)*100 #2.3 % of annual visitation

#-- CGR by land --
nps <- st_read("./data/GIS/NPS_boundary.shp") |> st_transform(5070)
st_crs(nps)
nps$area_m2 <- st_area(nps)
nps$acres <- nps$area_m2/4046.863
total_nps_acres <- sum(nps$acres, na.rm = T) #85,244,651 total acres in NPS lands

nps_im <- st_read("./data/GIS/CGI_parks_network.shp")
nps_im$area_m2 <- st_area(nps_im)
nps_im$acres <- nps_im$area_m2/4046.863
total_nps_acres_cgi <- sum(nps_im$acres, na.rm = T) #1,756,713 total acres in NPS lands
pct_nps_lands <- (total_nps_acres_cgi/total_nps_acres)*100 # = 2.06%


