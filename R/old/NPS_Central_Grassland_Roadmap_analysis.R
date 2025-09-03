#-------------------------------------------------------------------------------
# Identify parks within the Central Grasslands Roadmap boundary, and
# analyze proportion of habitat types for each park and buffered areas
# of each park. Code written by Kate Miller 7/24/2025
#-------------------------------------------------------------------------------
# Spatial analyses of habitat type use the Central Grasslands Assessment Map
# Downloaded on 7/24/2025 from: https://www.grasslandsroadmap.org/mapping
# https://worldwildlifefund-my.sharepoint.com/personal/sarah_olimb_wwfus_org/_layouts/15/guestaccess.aspx?folderid=048c87c2253764903b70978ed50f0a223&authkey=AeQ-cTZtSAH3AoulpLJ2w-4&e=ItzAFX

#---- Load libraries ----
library(tidyverse)
library(sf)
#library(raster)
library(terra)
library(tmap)
options(scipen = 100)
#sf_use_s2(FALSE)

#---- Part 1: Polygon of parks in CGI ----
# prep_data_for_shiny.R script contains the various rasters and shapefiles to
# compile the main datasets for spatial analysis
nps_im <- vect("./data_final/GIS/NPS_units_in_CGI_20250827_WGS84.shp")
st_crs(nps_im)

nps_im$area_ha <- round(expanse(nps_im, unit = "ha"), 2)
nps_im$area_acres <- round(nps_im$area_ha * 2.47105, 1)
head(nps_im)

#++++ ENDED HERE ++++ PROCESSING FILES IN PREP_DATA_FOR_SHINY (change that to analysis).
#-- Read in central grasslands tif and shapefile and reproject to match nps.
cgr_ras1 <- terra::rast("./data_final/GIS/CGR_GAM_V2_WGS84.tif")
cgr_ras <- terra::project(cgr_ras1, "EPSG:4326")
terra::writeRaster(cgr_ras, "./data_final/GIS/CGR_GAM_V2_WGS84.tif", overwrite = TRUE)
# DIDN'T ACTUALLY USE THE CODE BELOW- had to use ArcPro

# cgr_shp <- st_read("./data/GIS/Grasslands_Roadmap_boundary_Aug_2021.shp") |>
#   st_transform(crs = 5070)
#
# # Subtract 10km from buffer
# cgr_shp_buff <- st_buffer(cgr_shp, -10000) #10km
#
# # Check the layers
# tm_shape(cgr_shp) + tm_borders() +
#   tm_shape(cgr_shp_buff) + tm_borders('red') #
#
# # Intersect the parks with the cgr buffered polygon
# cgr_parks1 <- st_intersection(nps_im1, cgr_shp_buff)
# sort(unique(nps_im1$UNIT_CODE))
#
# sort(unique(cgr_parks1$UNIT_CODE))
# # calculate park area to see if some were partially cut
# cgr_parks1$area_m2_int <- st_area(cgr_parks1)
#
# # drop parks that aren't entirely within the boundary
# cgr_parks1 <- cgr_parks1 |> filter(!area_m2 > area_m2_int)
# cgr_park_list <- sort(cgr_parks1$UNIT_CODE)

# These are the parks to include in the NPS Central Grasslands Initiative
# But I want to include their entire boundary, so will filter by this park list
#nps_im <- nps_im1 |> filter(UNIT_CODE %in% cgr_park_list)

nps_im$acres <- round(nps_im$area_m2/4046.863, 2)
nps_im$ID <- 1:nrow(nps_im)

st_write(nps_im, "./data/GIS/CGI_parks_network.shp", append = FALSE)

# Creating buffered park boundaries to calculate raster statistics from
nps_im_1km <- st_buffer(nps_im, 1000)
st_write(nps_im_1km, "./data/GIS/CGI_parks_network_1km.shp", append = TRUE)

nps_im_10km <- st_buffer(nps_im, 10000)
st_write(nps_im_10km, "./data/GIS/CGI_parks_network_10km.shp", append = TRUE)

# Crop rasters to park and buffered park extents for faster processing.
#-- Only need to run once and slow, so commenting out --
cgr_ext_park <- crop(cgr_ras, nps_im, snap = 'in', mask = T)
writeRaster(cgr_ext_park, "./data/GIS/CGR_GAM_V2_park_extract_5070.tif")
cgr_ext_park1km <- crop(cgr_ras, nps_im_1km, snap = 'in', mask = T)
writeRaster(cgr_ext_park1km, "./data/GIS/CGR_GAM_V2_park1km_extract_5070.tif")
cgr_ext_park10km <- crop(cgr_ras, nps_im_10km, snap = 'in', mask = T)
writeRaster(cgr_ext_park10km, "./data/GIS/CGR_GAM_V2_park10km_extract_5070.tif")

#---- Part 2: Analyze habitat types by park ----
#-- Park Boundary --
# Switching to raster package because it worked better
cgr_ext_park <- raster::raster("./data/GIS/CGR_GAM_V2_park_extract_5070.tif")
dat <- levels(cgr_ext_park)[[1]]
dat$Category <- c("No Data", "Core Grassland", "Desert/Shrub",
                  "Vulnerable Grasslands",
                  "Converted/Altered Grasslands",
                  "Forest", "Developed", "Water")

# add habitat legend to raster attribute table
levels(cgr_ext_park)[[1]] <- dat
#prod(res(cgr_ext_park)) #1278.99 m2

# extract cells to values in a dataframe by nps unit
park_ext <- raster::extract(cgr_ext_park, nps_im, na.rm = T, df = T, exact = T)

park_ext_df <- as.data.frame(park_ext)

# setting up full table values and legend for join
cats <- data.frame(ID = c(0, 5, 7, 100, 500, 1000, 2000, 5000),
                   Habitat = c("No Data", "Core Grassland", "Desert/Shrub",
                               "Vulnerable Grasslands",
                               "Converted/Altered Grasslands",
                               "Forest", "Developed", "Water"))

# join extracted cgr values with nps_im data
park_ext2 <- left_join(park_ext_df, nps_im, by = "ID")
head(park_ext2)

# join with cgr values/habitat
park_ext3 <- full_join(park_ext2, cats, by = c("Category" = "ID"))
head(park_ext3)
# sum number of cells in each habitat type by park
park_ext4 <- park_ext3 |> group_by(ID, Habitat, UNIT_CODE, NETCODE) |>
  summarize(num_cells = n(), .groups = 'drop') |>
  filter(!is.na(ID))

# make nps_im a dataframe and drop geometry
nps_im_df <- as.data.frame(st_drop_geometry(nps_im)) |>
  filter(!(UNIT_CODE == "NEPE" & NETCODE == "ROMN"))
park_ext5 <- left_join(park_ext4, nps_im_df[,c("UNIT_CODE", "acres")], by = "UNIT_CODE") |> data.frame()

# Use pivots to get all combinations of habitat types for each park
park_ext6 <- park_ext5 |> dplyr::select(-ID) |>
  pivot_wider(names_from = Habitat, values_from = num_cells, values_fill = 0) |>
  pivot_longer(cols = 4:10, names_to = "Habitat", values_to = "num_cells")

park_ext7 <- park_ext6 |> group_by(UNIT_CODE, NETCODE, acres) |>
  mutate(total_cells = sum(num_cells, na.rm = T),
         prop_hab = (num_cells/total_cells)*100,
         acres_hab = (prop_hab/100) * acres,
         prop_check = sum(prop_hab)) |>
  arrange(UNIT_CODE, Habitat)

write.csv(park_ext7, "./data/CGR_parks_prop_habitat.csv")

#-- 1km buffer around park boundary --
cgr_ext_park_1km <- raster::raster("./data/GIS/CGR_GAM_V2_park1km_extract_5070.tif")
dat_1km <- levels(cgr_ext_park_1km)[[1]]
dat_1km$Category <- c("No Data", "Core Grassland", "Desert/Shrub",
                      "Vulnerable Grasslands",
                      "Converted/Altered Grasslands",
                      "Forest", "Developed", "Water")

# add habitat legend to raster attribute table
levels(cgr_ext_park_1km)[[1]] <- dat_1km

# extract cells to values in a dataframe by nps unit
park_ext_1km <- raster::extract(cgr_ext_park_1km, nps_im_1km, na.rm = T, df = T, exact = T)

park_ext_df_1km <- as.data.frame(park_ext_1km)

# setting up full table values and legend for join
cats <- data.frame(ID = c(0, 5, 7, 100, 500, 1000, 2000, 5000),
                   Habitat = c("No Data", "Core Grassland", "Desert/Shrub",
                               "Vulnerable Grasslands",
                               "Converted/Altered Grasslands",
                               "Forest", "Developed", "Water"))

# join extracted cgr values with nps_im data
park_ext2_1km <- left_join(park_ext_df_1km, nps_im_1km, by = "ID")

# join with cgr values/habitat
park_ext3_1km <- full_join(park_ext2_1km, cats, by = c("Category" = "ID"))

# sum number of cells in each habitat type by park
park_ext4_1km <- park_ext3_1km |> group_by(ID, Habitat, UNIT_CODE, NETCODE) |>
  summarize(num_cells = n(), .groups = 'drop') |>
  filter(!is.na(ID))

# make nps_im a dataframe and drop geometry
nps_im_df_1km <- as.data.frame(st_drop_geometry(nps_im_1km))
park_ext5_1km <- left_join(park_ext4_1km, nps_im_df_1km[,c("UNIT_CODE", "acres")],
                           by = "UNIT_CODE") |> data.frame()

# Use pivots to get all combinations of habitat types for each park
park_ext6_1km <- park_ext5_1km |> dplyr::select(-ID) |>
  pivot_wider(names_from = Habitat, values_from = num_cells, values_fill = 0) |>
  pivot_longer(cols = 4:10, names_to = "Habitat", values_to = "num_cells")

park_ext7_1km <- park_ext6_1km |> group_by(UNIT_CODE, NETCODE, acres) |>
  mutate(total_cells = sum(num_cells, na.rm = T),
         prop_hab = (num_cells/total_cells)*100,
         acres_hab = (prop_hab/100) * acres,
         prop_check = sum(prop_hab)) |>
  arrange(UNIT_CODE, Habitat)

write.csv(park_ext7_1km, "./data/CGR_parks_prop_habitat_1km.csv")

#-- 10km buffer around park boundary --
cgr_ext_park_10km <- raster::raster("./data/GIS/CGR_GAM_V2_park10km_extract_5070.tif")
dat_10km <- levels(cgr_ext_park_10km)[[1]]
dat_10km$Category <- c("No Data", "Core Grassland", "Desert/Shrub",
                      "Vulnerable Grasslands",
                      "Converted/Altered Grasslands",
                      "Forest", "Developed", "Water")

# add habitat legend to raster attribute table
levels(cgr_ext_park_10km)[[1]] <- dat_10km

# extract cells to values in a dataframe by nps unit
park_ext_10km <- raster::extract(cgr_ext_park_10km, nps_im_10km, na.rm = T, df = T, exact = T)

park_ext_df_10km <- as.data.frame(park_ext_10km)

# setting up full table values and legend for join
cats <- data.frame(ID = c(0, 5, 7, 100, 500, 1000, 2000, 5000),
                   Habitat = c("No Data", "Core Grassland", "Desert/Shrub",
                               "Vulnerable Grasslands",
                               "Converted/Altered Grasslands",
                               "Forest", "Developed", "Water"))

# join extracted cgr values with nps_im data
park_ext2_10km <- left_join(park_ext_df_10km, nps_im_10km, by = "ID")

# join with cgr values/habitat
park_ext3_10km <- full_join(park_ext2_10km, cats, by = c("Category" = "ID"))

# sum number of cells in each habitat type by park
park_ext4_10km <- park_ext3_10km |> group_by(ID, Habitat, UNIT_CODE, NETCODE) |>
  summarize(num_cells = n(), .groups = 'drop') |>
  filter(!is.na(ID))

# make nps_im a dataframe and drop geometry
nps_im_df_10km <- as.data.frame(st_drop_geometry(nps_im_10km))
park_ext5_10km <- left_join(park_ext4_10km, nps_im_df_10km[,c("UNIT_CODE", "acres")],
                           by = "UNIT_CODE") |> data.frame()

# Use pivots to get all combinations of habitat types for each park
park_ext6_10km <- park_ext5_10km |> dplyr::select(-ID) |>
  pivot_wider(names_from = Habitat, values_from = num_cells, values_fill = 0) |>
  pivot_longer(cols = 4:10, names_to = "Habitat", values_to = "num_cells")

park_ext7_10km <- park_ext6_10km |> group_by(UNIT_CODE, NETCODE, acres) |>
  mutate(total_cells = sum(num_cells, na.rm = T),
         prop_hab = (num_cells/total_cells)*100,
         acres_hab = (prop_hab/100) * acres,
         prop_check = sum(prop_hab)) |>
  arrange(UNIT_CODE, Habitat)

write.csv(park_ext7_10km, "./data/CGR_parks_prop_habitat_10km.csv")
