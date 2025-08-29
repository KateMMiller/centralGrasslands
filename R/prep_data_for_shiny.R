#-------------------------------------------------------
# prepping code for R Shiny and use with leaflet (wgs84)
#-------------------------------------------------------
library(raster)
library(sf)
library(tidyverse)
#library(spatialEco)
library(terra)

sf_use_s2(FALSE)

#-- Generated the shapefile below from NPS Administrative Boundaries,
# and selected only parks in CGI, then converted to WGS84 in ArcGIS Pro
nps_im <- st_read("./data_final/GIS/NPS_units_in_CGI_20250827_WGS84_pre.shp")
nps_im_cent <- st_centroid(nps_im)
nps_cent_latlong <- data.frame(st_drop_geometry(nps_im_cent),
                               long = st_coordinates(nps_im_cent)[,1],
                               lat = st_coordinates(nps_im_cent)[,2]
                               )
networks <- st_read("./data_final/GIS/networks_wgs84.shp")

# Add centroid to attribute table
nps_im2 <- left_join(nps_im, nps_cent_latlong[,c("UNIT_CODE", "long", "lat")],
                     by = c("UNIT_CODE"))

# Add network info via csv vetted by IMD protocol leads
imd_info <- read.csv("./data_final/IMD_networks_grassland_monitoring_info_20250827.csv")

nps_im3 <- left_join(nps_im2, imd_info, by = c("UNIT_CODE" = "Park"))
nps_im_val <- st_make_valid(nps_im3) |> select(-GIS_Notes, -METADATA, -GlobalID, -Editor,
                                               -Shape_Area, -long.x, -lat.x) |>
  rename(long = long.y, lat = lat.y)

head(nps_im_val)

st_write(nps_im_val, "./data_final/GIS/NPS_units_in_CGI_20250827_WGS84.shp", append = FALSE)
nps_im_val <- st_read("./data_final/GIS/NPS_units_in_CGI_20250827_WGS84.shp")

#create buffers
sf_use_s2(TRUE) # so dist is in m
nps_im_1km <- st_buffer(nps_im_val, dist = 1000)
nps_im_10km <- st_buffer(nps_im_val, dist = 10000)

st_write(nps_im_1km, "./data_final/GIS/NPS_units_in_CGI_20250827_WGS84_1km.shp", append = FALSE)
st_write(nps_im_10km, "./data_final/GIS/NPS_units_in_CGI_20250827_WGS84_10km.shp", append = FALSE)

cgr_border <- st_read("./data_final/GIS/Grasslands_Roadmap_boundary_Aug_2021_WGS84.shp")

# Commenting out after first run
# Clipping CGR raster the 10km buffer of parks
# cgr_ras <- raster::raster("./data_final/GIS/CGR_GAM_V2_WGS84.tif")
# cgr_10km <- mask(cgr_ras, nps_im_10km)
# writeRaster(cgr_10km, "./data_final/GIS/CGR_GAM_V2_WGS84_10km.tif")

# Converting CGR 10km raster to polygons via terra package
# nps_im_terra <- terra::vect("./data_final/GIS/NPS_units_in_CGI_20250827_wgs84.shp")
# cgr_ras <- terra::rast("./data_final/GIS/CGR_GAM_V2_WGS84_10km.tif")
# cgr_ras_wgs <- terra::project(cgr_ras, "EPSG:4326")
# writeRaster(cgr_ras_wgs, "./data_final/GIS/CGR_GAM_V2_WGS84_10km.tif", overwrite = T)
#
# cgr_shp <- terra::as.polygons(cgr_ras_wgs, round = T, aggregate = T, values = T, na.rm = T)
# writeVector(cgr_shp, "./data_final/CGR_GAM_V2_WGS84_10km.shp", overwrite = T)
# cgr_shp_simp <- terra::simplifyGeom(cgr_shp, tolerance = 0.001, preserveTopology = TRUE, makeValid = T)
# writeVector(cgr_shp_simp, "./data_final/CGR_GAM_V2_WGS84_10km_simp.shp", overwrite = T)

cgr_shp <- terra::vect("./data_final/GIS/CGR_GAM_V2_WGS84_10km.shp")

# Clipping CGR raster to boundary of parks
cgr_ras_orig <- terra::rast("./data_final/GIS/CGR_GAM_V2_WGS84.tif")
cgr_ras_orig_wgs <- terra::project(cgr_ras_orig, "EPSG:4326")
cgr_nps <- terra::mask(cgr_ras_orig_wgs, nps_im_val)
terra::writeRaster(cgr_nps, "./data_final/GIS/CGR_GAM_V2_WGS84_parks.tif", overwrite = T)

#-- Analyze habitat types by park ----
# Switching to raster package because it worked better
cgr_ext_park <- raster::raster("./data_final/GIS/CGR_GAM_V2_WGS84_parks.tif")
dat <- levels(cgr_ext_park)[[1]] #++++ RUNNING HERE ++++
dat$Category <- c("No Data", "Core Grassland", "Desert/Shrub",
                  "Vulnerable Grasslands",
                  "Converted/Altered Grasslands",
                  "Forest", "Developed", "Water")

# add habitat legend to raster attribute table
levels(cgr_ext_park)[[1]] <- dat
#prod(res(cgr_ext_park)) #1278.99 m2

# extract cells to values in a dataframe by nps unit
park_ext <- raster::extract(cgr_ext_park, nps_im_val, na.rm = T, df = T, exact = T)

park_ext_df <- as.data.frame(park_ext)

# setting up full table values and legend for join
cats <- data.frame(ID = c(0, 5, 7, 100, 500, 1000, 2000, 5000),
                   Habitat = c("No Data", "Core Grassland", "Desert/Shrub",
                               "Vulnerable Grasslands",
                               "Converted/Altered Grasslands",
                               "Forest", "Developed", "Water"))

# join extracted cgr values with nps_im data
nps_im_val$ID <- 1:nrow(nps_im_val)
park_ext2 <- left_join(park_ext_df, nps_im_val, by = "ID") #+++++ Running here ++++++
#+++++ ENDED HERE ON CODE UPDATE +++++
head(park_ext2)

# join with cgr values/habitat
park_ext3 <- full_join(park_ext2, cats, by = c("Category" = "ID"))
head(park_ext3)
# sum number of cells in each habitat type by park
park_ext4 <- park_ext3 |> group_by(ID, Habitat, UNIT_CODE, NETCODE) |>
  summarize(num_cells = n(), .groups = 'drop') |>
  filter(!is.na(ID))

# make nps_im a dataframe and drop geometry
nps_im_df <- as.data.frame(st_drop_geometry(nps_im_val))

#park_ext5 <- left_join(park_ext4, nps_im_df[,c("UNIT_CO", "acres")], by = "UNIT_CODE") |> data.frame()

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

#+++++ ENDED HERE ++++++ #+++ RUNNING HERE +++



cgr_shp <- st_read("./data/GIS/CGR_GAM_V2_UTM_NAD83_10km.shp") |> st_transform(4326)
st_write(cgr_shp, "./data/GIS/CGR_GAM_V2_WGS_10km.shp", append = F)

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

# Identify non-CGR parks and make their prop data NA
non_cgr_parks <- c("AZRU", "BAND", "BICA", "EFMO", "ELMA", "GLAC", "GRSA", "IATR", "NEPE",
                   "PECO", "PEFO", "PERI", "PETR", "VALL", "WUPA")
park_prop_comb$CGR_park <- ifelse(park_prop_comb$UNIT_CODE %in% non_cgr_parks, 0, 1)
hab_cols <- c("prop_hab", "acres_hab", "prop_hab_1km",
              "acres_hab_1km", "prop_hab_10km", "acres_hab_10km")

park_prop_comb[park_prop_comb$Non_CGR == 0, hab_cols] <- NA_real_

write.csv(park_prop_comb, "./data/GIS/CGR_parks_prop_habitat_all.csv")

#-- Visitation statistics --
# Data downloaded for all NPS units on 7/24/2024:
# https://irma.nps.gov/Stats/SSRSReports/National%20Reports/Query%20Builder%20for%20Public%20Use%20Statistics%20(1979%20-%20Last%20Calendar%20Year)

vis <- read.csv('./data/NPS_Public_Use_Statistics_2024.csv')
head(vis)
# total visitation
total_vis <- sum(vis$Recreation.Visits[-nrow(vis)], na.rm = T) #331,863,358
# cgr park visitation
cgr_vis <- sum(vis$Recreation.Visits[vis$CGI_Park == "X"], na.rm = T) #15,367,168
pct_vis <- (cgr_vis/total_vis)*100 #4.63 % of annual visitation

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
pct_nps_lands <- (total_nps_acres_cgi/total_nps_acres)*100 # = 2.98%


