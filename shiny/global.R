#-------------------------------------------------------------------
# Global file loads objects available to server and ui
#-------------------------------------------------------------------
library(sf)
library(raster)
library(leaflet)
library(tidyr)
library(dplyr)
sf_use_s2(FALSE)
#library(crosstalk)
path = "C:/NETN/R_Dev/centralGrasslands/data/GIS/"

# WGS 84 version of datasets
nps_im <- st_read(paste0(path, "CGI_parks_network_wgs.shp"))
#nps_bbox1 <- st_bbox(nps_im) * 1.01 # added 1% buffer
nps_bbox <- data.frame(xmin = -113.16628, ymin = 29.38215,
                       xmax = -95.35811, ymax = 47.85992)

# long_mean = mean(nps_bbox$xmin, nps_bbox$xmax)
# lat_mean = mean(nps_bbox$ymin, nps_bbox$ymax)

#cent1 <- st_coordinates(st_centroid(st_as_sfc(nps_bbox)))
cent <- data.frame(long = -104.2622, lat = 38.62103)
nps_im_df <- data.frame(st_drop_geometry(nps_im))
names(nps_im_df)[names(nps_im_df) == "UNIT_NA"] <- "UNIT_NAME"
names(nps_im_df)[names(nps_im_df) == "UNIT_CO"] <- "UNIT_CODE"

nps_im_1km <- st_read(paste0(path, "CGI_parks_network_1km_wgs.shp"))
nps_im_10km <- st_read(paste0(path, "CGI_parks_network_10km_wgs.shp"))
#cgr_ras <- raster::raster("./data/GIS/CGR_GAM_V2_WGS84.tif")

#cgr_shp <- st_read("./data/GIS/CGR_GAM_V2_WGS84.shp")
park_prop_hab <- read.csv(paste0(path, "CGR_parks_prop_habitat_all.csv"))
round_cols <- c("acres", "prop_hab", "acres_hab", "prop_hab_1km", "acres_hab_1km", "prop_hab_10km",
                "acres_hab_10km")
park_prop_hab[,round_cols] <- round(park_prop_hab[,round_cols], 1)
park_prop_hab$Habitat <- gsub("/", "_", park_prop_hab$Habitat)
park_prop_hab$Habitat <- gsub(" ", "_", park_prop_hab$Habitat)

park_prop_hab_wide <- park_prop_hab |> dplyr::select(UNIT_CODE:acres_hab) |>
  pivot_wider(names_from = Habitat, values_from = c(prop_hab, acres_hab))
names(park_prop_hab_wide) <- gsub("_hab", "", names(park_prop_hab_wide))
head(data.frame(park_prop_hab_wide))

park_prop_hab_wide2 <- left_join(park_prop_hab_wide,
                                 nps_im_df[,c("UNIT_CODE", "UNIT_NAME", "long", "lat")],
                                 by = c('UNIT_CODE')) |>
  mutate(pie_size1 = 2*sqrt(acres/sqrt(max(acres))),
         pie_size = ifelse(pie_size1 < 10, 10,
                           ifelse(pie_size1 > 35, 35, pie_size1)),
         zoom = case_when(acres > 100000 ~ 16,
                          between(acres, 50000, 100000) ~ 12,
                          between(acres, 10000, 50000) ~ 10,
                          between(acres, 5000,10000) ~ 7,
                          between(acres, 1000,5000) ~ 5,
                          between(acres, 500, 1000) ~ 3,
                          between(acres, 100, 500) ~ 2,
                          acres < 100 ~ 1))
# names(park_prop_hab_wide2)[names(park_prop_hab_wide2) == "UNIT_NA"] <- "UNIT_NAME"

park_prop <- park_prop_hab_wide2[,c("UNIT_CODE", "UNIT_NAME", "NETCODE", "acres",
                                    "prop_Core_Grassland", "prop_Vulnerable_Grasslands",
                                    "prop_Converted_Altered_Grasslands", "prop_Desert_Shrub",
                                    "prop_Forest", "prop_Developed", "prop_Water",
                                    "acres_Core_Grassland", "acres_Vulnerable_Grasslands",
                                    "acres_Converted_Altered_Grasslands", "acres_Desert_Shrub",
                                    "acres_Forest", "acres_Developed", "acres_Water",
                                    "long", "lat", "zoom")]
park_prop2 <- park_prop |> dplyr::select(-UNIT_NAME, -zoom)

network_list <- sort(unique(nps_im$NETCODE))
park_list <- sort(unique(nps_im$UNIT_CODE))

# Cross# Cross# Crosstalk code
#df_shared <- SharedData$new(park_prop2)

cgr_bound <- st_read(paste0(path, "Grasslands_Roadmap_boundary_Aug_2021_WGS84.shp"))

#cgr_shp <- st_read("./data/GIS/CGR_GAM_V2_10km_WGS84_diss.shp")
# cgr_ras <- terra::rast("./data/GIS/CGR_GAM_V2_park10km_extract.tif")

