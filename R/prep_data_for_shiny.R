#-------------------------------------------------------
# prepping code for R Shiny and use with leaflet (wgs84)
#-------------------------------------------------------
library(raster)
library(sf)
library(tidyverse)
#library(spatialEco)
library(terra)

sf_use_s2(FALSE)
options(scipen = 100)
#-- Generated the shapefile below from NPS Administrative Boundaries,
# and selected only parks in CGI, then converted to WGS84 in ArcGIS Pro
nps_im1 <- st_read("./data_final/GIS/NPS_units_in_CGI_20250827_WGS84_pre.shp") |> select(-lat, -long)
nps_im <- st_make_valid(nps_im1)
nps_im_cent <- st_centroid(nps_im)
nps_cent_latlong <- data.frame(st_drop_geometry(nps_im_cent),
                               long = st_coordinates(nps_im_cent)[,1],
                               lat = st_coordinates(nps_im_cent)[,2]
                               )
networks <- st_read("./data_final/GIS/networks_wgs84.shp")

# Add centroid to attribute table
nps_im2 <- left_join(nps_im, nps_cent_latlong[,c("UNIT_CODE", "long", "lat")],
                     by = c("UNIT_CODE"))
head(nps_im2)
nps_im$area_m2 <- st_area(nps_im)
nps_im2$Acres <- nps_im2$area_m2/4046.863

# Add network info via csv vetted by IMD protocol leads
imd_info <- read.csv("./data_final/IMD_networks_grassland_monitoring_info_20250827.csv")

nps_im3 <- left_join(nps_im2, imd_info, by = c("UNIT_CODE" = "Park")) |>
  select(-GIS_Notes, -METADATA, -GlobalID, -Editor, -Shape_Area)

st_write(nps_im3, "./data_final/GIS/NPS_units_in_CGI_20250827_WGS84.shp", append = FALSE)
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
# writeVector(cgr_shp, "./data_final/GIS/CGR_GAM_V2_WGS84_10km.shp", overwrite = T)
# cgr_shp_simp <- terra::simplifyGeom(cgr_shp, tolerance = 0.001, preserveTopology = TRUE, makeValid = T)
# writeVector(cgr_shp_simp, "./data_final/GIS/CGR_GAM_V2_WGS84_10km_simp.shp", overwrite = T)

cgr_shp <- terra::vect("./data_final/GIS/CGR_GAM_V2_WGS84_10km.shp")

# Clipping CGR raster to boundary of parks
cgr_ras_orig <- terra::rast("./data_final/GIS/CGR_GAM_V2_WGS84.tif")
cgr_ras_orig_wgs <- terra::project(cgr_ras_orig, "EPSG:4326")
cgr_nps <- terra::mask(cgr_ras_orig_wgs, nps_im_val)
terra::writeRaster(cgr_nps, "./data_final/GIS/CGR_GAM_V2_WGS84_parks.tif", overwrite = T)

#-- Analyze habitat types by park ----
# Switching to raster package because it worked better
cgr_ext_park <- raster::raster("./data_final/GIS/CGR_GAM_V2_WGS84_parks.tif")
dat <- levels(cgr_ext_park)[[1]]
dat$Category <- c("No Data", "Core Grassland", "Desert/Shrub",
                  "Vulnerable Grasslands",
                  "Converted/Altered Grasslands",
                  "Forest", "Developed", "Water")

# add habitat legend to raster attribute table
levels(cgr_ext_park)[[1]] <- dat
#prod(res(cgr_ext_park)) #1278.99 m2

# extract cells to values in a dataframe by nps unit
nps_im_val <- st_read("./data_final/GIS/NPS_units_in_CGI_20250827_WGS84.shp")
park_ext <- raster::extract(cgr_ext_park, nps_im_val, na.rm = T, df = T, exact = T)
park_ext_df <- as.data.frame(park_ext)
colnames(park_ext_df) <- c("ID", "Category")

# setting up full table values and legend for join
cats <- data.frame(ID = c(0, 5, 7, 100, 500, 1000, 2000, 5000),
                   Habitat = c("No Data", "Core Grassland", "Desert/Shrub",
                               "Vulnerable Grasslands",
                               "Converted/Altered Grasslands",
                               "Forest", "Developed", "Water"))

# join extracted cgr values with nps_im data
nps_im_val$ID <- 1:nrow(nps_im_val)
head(park_ext_df)

park_ext2 <- left_join(park_ext_df, nps_im_val, by = "ID")
head(park_ext2)

# join with cgr values/habitat
park_ext3 <- full_join(park_ext2, cats, by = c("Category" = "ID"))
names(park_ext3)

# sum number of cells in each habitat type by park
park_ext4 <- park_ext3 |> group_by(ID, Category, UNIT_CO, Network, IM_Veg_Mon = IM_gr__) |>
  summarize(num_cells = n(), .groups = 'drop') |>
  filter(!is.na(ID))

# make nps_im a dataframe and drop geometry
nps_im_df <- as.data.frame(st_drop_geometry(nps_im_val))

park_ext5 <- left_join(park_ext4, nps_im_df[,c("UNIT_CO", "Acres")], by = "UNIT_CO") |> data.frame()

# Use pivots to get all combinations of habitat types for each park
park_ext6 <- park_ext5 |> dplyr::select(-ID) |>
  pivot_wider(names_from = Category, values_from = num_cells, values_fill = 0)

park_ext7 <-
  if(any(names(park_ext6) %in% "NA")){park_ext6 |> select(-`NA`) |>
  pivot_longer(cols = 5:12, names_to = "Habitat_Type", values_to = "num_cells")
} else {park_ext6 |> pivot_longer(cols = 5:12, names_to = "Habitat_Type", values_to = "num_cells") }

park_ext8 <- park_ext7 |> group_by(UNIT_CODE = UNIT_CO, Network, Acres, IM_Veg_Mon) |>
  mutate(total_cells = sum(num_cells, na.rm = T),
         prop_hab = (num_cells/total_cells)*100,
         acres_hab = (prop_hab/100) * Acres,
         prop_check = sum(prop_hab)) |>
  arrange(UNIT_CODE, Habitat_Type) |>
  ungroup()

cats$ID <- as.character(cats$ID)
park_ext9 <- left_join(park_ext8, cats, by = c("Habitat_Type" = "ID")) |>
  select(UNIT_CODE = UNIT_CO, Network, IM_Veg_Mon, Acres, Habitat_Code = Habitat_Type,
         Habitat, prop_hab, acres_hab)

# add column for parks that are 90% not in CGR
ggplot(park_ext9 |> filter(Habitat_Code == "0"), aes(x = UNIT_CODE, y = prop_hab)) +
  geom_point() + facet_wrap(~Network, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(yintercept = 10)

# Parks not >=90% CGR:
nonCGR_parks <- park_ext9 |> ungroup() |>
  filter(Habitat_Code == "0" & prop_hab > 10) |> select(UNIT_CODE) |>
  arrange(UNIT_CODE)

nonCGR <- sort(c(nonCGR_parks$UNIT_CODE))

park_ext9$CGR_park <- ifelse(park_ext9$UNIT_CODE %in% nonCGR, 0, 1)
# "ARCH" "AZRU" "BAND" "BICA" "CANY" "CARE" "CHCU"
# "EFMO" "ELMA" "GRKO" "GRSA" "GWCA" "IATR" "NATR" "NEPE"
# "PAIS" "PECO" "PEFO" "PETR" "VALL" "WICR" WUPA"

hab_cols <- c("prop_hab", "acres_hab")
park_ext9[park_ext9$CGR_park == 0, hab_cols] <- NA_real_
write.csv(park_ext9, "./data_final/CGR_parks_prop_habitat.csv")

#-- Visitation statistics --
# Data downloaded for all NPS units on 7/24/2024:
# https://irma.nps.gov/Stats/SSRSReports/National%20Reports/Query%20Builder%20for%20Public%20Use%20Statistics%20(1979%20-%20Last%20Calendar%20Year)
nps_im <- st_read("./data_final/GIS/NPS_units_in_CGI_20250827_WGS84.shp")
park_prop_hab <- read.csv("./data_final/CGR_parks_prop_habitat.csv")
vis <- read.csv('./data_final/NPS_Public_Use_Statistics_2024.csv')

# total visitation
total_vis <- sum(vis$Recreation.Visits[-nrow(vis)], na.rm = T) #331,863,358
# cgr park visitation
cgr_vis <- sum(vis$Recreation.Visits[vis$CGI_Park == "X"], na.rm = T) #29,522,897
pct_vis <- (cgr_vis/total_vis)*100 #8.89 % of annual visitation

#-- CGR by land --
nps <- st_read("./data_final/GIS/NPS_boundary.shp") |> st_transform(5070)
st_crs(nps)
nps$area_m2 <- st_area(nps)
nps$acres <- nps$area_m2/4046.863
total_nps_acres <- sum(nps$acres, na.rm = T) #85,244,651 total acres in NPS lands

nps_im <- st_read("./data_final/GIS/NPS_units_in_CGI_20250827_WGS84.shp")
nps_im$area_m2 <- st_area(nps_im)
nps_im$acres <- nps_im$area_m2/4046.863
total_nps_acres_cgi <- sum(nps_im$acres, na.rm = T) #3,373,394 total acres in NPS lands
pct_nps_lands <- (total_nps_acres_cgi/total_nps_acres)*100 # = 3.96%

# add info to park data for shiny data.table
nps_im2 <- left_join(nps_im, vis[,c("Code", "Recreation.Visits")], by = c("UNIT_CO" = "Code"))
head(nps_im2)
round_cols <- c("Acres", "prop_hab", "acres_hab")
park_prop_hab[,round_cols] <- round(park_prop_hab[,round_cols], 1)
park_prop_hab$Habitat <- gsub("/", "_", park_prop_hab$Habitat)
park_prop_hab$Habitat <- gsub(" ", "_", park_prop_hab$Habitat)

park_prop_wide <- park_prop_hab |> select(UNIT_CODE, IM_Veg_Mon, CGR_park, Habitat, prop_hab, acres_hab) |>
  pivot_wider(names_from = Habitat, values_from = c(prop_hab, acres_hab))

names(park_prop_wide) <- gsub("_hab", "", names(park_prop_wide))

# Add leaflet zoom
park_prop_wide2 <- left_join(nps_im2[,c("UNIT_CO", "UNIT_NA", "Network", "long", "lat", "Acres",
                                        "Recreation.Visits")],
                             park_prop_wide,
                             by = c('UNIT_CO' = 'UNIT_CODE')) |>
  mutate(#pie_size1 = 2*sqrt(Acres/sqrt(max(Acres))),
         # pie_size = ifelse(pie_size1 < 10, 10,
         #                   ifelse(pie_size1 > 35, 35, pie_size1)),
         zoom = case_when(Acres > 100000 ~ 16,
                          between(Acres, 50000, 100000) ~ 12,
                          between(Acres, 10000, 50000) ~ 10,
                          between(Acres, 5000,10000) ~ 7,
                          between(Acres, 1000,5000) ~ 5,
                          between(Acres, 500, 1000) ~ 3,
                          between(Acres, 100, 500) ~ 2,
                          Acres < 100 ~ 1)) |> #|> select(-pie_size1)
  arrange(UNIT_CO)

st_write(park_prop_wide2, "./data_final/GIS/NPS_units_in_CGI_20250903_WGS84.shp", append = F)

park_prop_df <- data.frame(st_drop_geometry(park_prop_wide2)) |>
  select(Unit_Code = UNIT_CO, Unit_Name = UNIT_NA, Network, CGR_park, Acres,
         Visitation_2024 = Recreation.Visits, IM_Veg_Mon,
         prop_Core_Grassland, prop_Vulnerable_Grasslands, prop_Converted_Altered_Grasslands,
         prop_Desert_Shrub, prop_Developed, prop_Forest, prop_Water,
         acres_Core_Grassland, acres_Vulnerable_Grasslands, acres_Converted_Altered_Grasslands,
         acres_Desert_Shrub, acres_Developed, acres_Forest, acres_Water,
         lat, long, zoom)

write.csv(park_prop_df, "./data_final/CGR_parks_prop_habitat.csv")

