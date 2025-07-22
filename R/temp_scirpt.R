#---------------------------------
# Central grassland data compilation
library(tidyverse)
library(sf)
#library(raster)
library(terra)
library(tmap)
options(scipen = 100)
# Determining parks that are within the central grassland roadmap boundary
# Read in NPS boundary layer downloaded from IRMA on 7/22/2025
nps <- st_read("./data/GIS/nps_boundary.shp")
st_crs(nps)
nps$area_m2 <- st_area(nps)

# Read in network boundaries and transform to nps
imd <- st_read("./data/GIS/networks.shp") |> st_transform(crs = 4269)
st_crs(nps) == st_crs(imd)
names(imd)

nps_im <- st_join(nps, imd) |>
  dplyr::select(UNIT_CODE, UNIT_NAME, STATE, REGION, area_m2, NETWORK = NAME2_, NETCODE = ALPHACODE)

#-- Read in central grasslands tif and shapefile and reproject to match nps.
#-- Only need to do this once
# cgr_ras <- rast("./data/GIS/CGR_GAM_V2.tif")
# cgr_ras83 <- project(cgr_ras, crs(nps))
# writeRaster(cgr_ras83, "./data/GIS/CGR_GAM_V2_NAD83.tif")

cgr_ras <- rast("./data/GIS/CGR_GAM_V2_NAD83.tif")

cgr_shp <- st_read("./data/GIS/Grasslands_Roadmap_boundary_Aug_2021.shp") |>
  st_transform(crs = 4269)

# convert m to degrees
buff_dist <- -(1000 * 360 / (2*pi * 6400000)) # 1km
cgr_shp_buff <- st_buffer(cgr_shp, -0.5)

tm_shape(cgr_shp) + tm_borders() +
  tm_shape(cgr_shp_buff) + tm_borders('red') #0.5 degrees

sf_use_s2(FALSE)
cgr_parks1 <- st_intersection(nps_im, cgr_shp_buff)
cgr_parks1$area_m2_int <- st_area(cgr_parks1)

# drop parks that aren't entirely within the boundary
cgr_parks <- cgr_parks1 |> filter(!area_m2 > area_m2_int)
View(cgr_parks)

#tm_shape(imd, bbox = cgr_shp) + tm_borders() +
  tm_shape(cgr_parks) + tm_polygons(col = 'red') +
  tm_shape(cgr_shp) + tm_borders()

table(cgr_parks$NETCODE)



