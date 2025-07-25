#---------------------------------
# Central grassland data compilation
library(tidyverse)
library(sf)
#library(raster)
library(terra)
library(tmap)
options(scipen = 100)
sf_use_s2(FALSE)
# Determining parks that are within the central grassland roadmap boundary
# Read in NPS boundary layer downloaded from IRMA on 7/22/2025
nps <- st_read("./data/GIS/nps_boundary.shp") |> st_transform(5070) # Conus Albers NAD83
st_crs(nps)
nps$area_m2 <- st_area(nps)

# Read in network boundaries and transform to nps
imd <- st_read("./data/GIS/networks.shp") |> st_transform(crs = 5070)
st_crs(nps) == st_crs(imd)
names(imd)

nps_im1 <- st_join(nps, imd) |>
  dplyr::select(UNIT_CODE, UNIT_NAME, STATE, REGION, area_m2, NETWORK = NAME2_, NETCODE = ALPHACODE)

#-- Read in central grasslands tif and shapefile and reproject to match nps.
#-- Only need to do this once
# cgr_ras <- rast("./data/GIS/CGR_GAM_V2.tif")
# cgr_ras83 <- project(cgr_ras, crs(nps), threads = 20)
# crs(cgr_ras83)
# writeRaster(cgr_ras83, "./data/GIS/CGR_GAM_V2_UTMNAD83.tif")

cgr_ras <- rast("./data/GIS/CGR_GAM_V2_UTM_NAD83.tif")

cgr_shp <- st_read("./data/GIS/Grasslands_Roadmap_boundary_Aug_2021.shp") |>
  st_transform(crs = 5070)

# Subtract 10km from buffer
cgr_shp_buff <- st_buffer(cgr_shp, -10000) #10km

tm_shape(cgr_shp) + tm_borders() +
  tm_shape(cgr_shp_buff) + tm_borders('red') #

# Intersect the parks with the cgr buffered polygon
cgr_parks1 <- st_intersection(nps_im1, cgr_shp_buff)
# calculate park area to see if some were partially cut
cgr_parks1$area_m2_int <- st_area(cgr_parks1)

# drop parks that aren't entirely within the boundary
cgr_parks1 <- cgr_parks1 |> filter(!area_m2 > area_m2_int)
cgr_park_list <- sort(cgr_parks1$UNIT_CODE)
# These are the parks to include in the NPS Central Grasslands Initiative
# But I want to include their entire boundary, so will filter by this park list
nps_im <- nps_im1 |> filter(UNIT_CODE %in% cgr_park_list)
nps_im$acres <- nps_im$area_m2 * 0.000247105

st_write(nps_im, "./data/GIS/CGI_parks_network.shp")
#plot(nps_im[1])
# Creating buffered park boundaries to calculate raster statistics from
# nps_im_1km <- st_buffer(nps_im, 1000)
# nps_im_10km <- st_buffer(nps_im, 10000)

nps_imv <- vect(nps_im)

# cgr_ext_park <- crop(cgr_ras, nps_im, snap = 'in', mask = T)
# writeRaster(cgr_ext_park, "./data/GIS/CGR_GAM_V2_park_extract.tif")
# cgr_ext_park1km <- crop(cgr_ras, nps_im_1km, snap = 'in', mask = T)
# writeRaster(cgr_ext_park1km, "./data/GIS/CGR_GAM_V2_park1km_extract.tif")
# cgr_ext_park10km <- crop(cgr_ras, nps_im_10km, snap = 'in', mask = T)
# writeRaster(cgr_ext_park10km, "./data/GIS/CGR_GAM_V2_park10km_extract.tif")

cgr_ext_park <- rast("./data/GIS/CGR_GAM_V2_park_extract.tif")
cgr_ext_park[[1]]$Category <- c("No Data", "Core Grassland", "Desert/Shrub",
                                "Vulnerable Grasslands",
                                "Converted/Altered Grasslands",
                                "Forest", "Developed", "Water")

dim(cgr_ext_park[[1]])
levels(cgr_ext_park[[1]])
#cgr_ext_park1km <- rast("./data/GIS/CGR_GAM_V2_park1km_extract.tif")
#cgr_ext_park10km <- rast("./data/GIS/CGR_GAM_V2_park10km_extract.tif")

nps_im$ID <- 1:nrow(nps_im)

cgr_ext_park2 <- merge(cgr_ext_park, nps_im, by = "ID")

cats <- data.frame(ID = c(0, 5, 7, 100, 500, 1000, 2000, 5000),
                   Category = c("No Data", "Core Grassland", "Desert/Shrub",
                                "Vulnerable Grasslands",
                                "Converted/Altered Grasslands",
                                "Forest", "Developed", "Water"))

head(cgr_ext_park2)

table(cgr_park_ext$ID)

hist(cgr_ext_park)

cgr_park_ext <- extract(cgr_ext_park, nps_im, na.rm = T, exact = T)
head(cgr_park_ext)

cgr_park_ext$park <- nps_im$UNIT_CODE

plot(cgr_ext_park)
cgr_ext_park
tm_shape(nps_im |> filter(NETCODE == "NGPN")) + tm_polygons(col = 'red') +
  tm_shape(nps_im_1km) + tm_borders(col = 'blue') +
  tm_shape(nps_im_10km) + tm_borders(col = 'black') +
  tm_shape(cgr_shp) + tm_borders()

st_write(nps_im, "./data/GIS/CGR_park_units_5070.shp")
warnings()
table(nps_im$NETCODE)
table(nps_im$UNIT_CODE)
# 28 parks
cgr_ras$CGR_GAM_V2


