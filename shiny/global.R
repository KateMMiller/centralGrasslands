#-------------------------------------------------------------------
# Global file loads objects available to server and ui
#-------------------------------------------------------------------
# options(repos = "https://rspatial.r-universe.dev")
# install.packages('quarto',
#                 repos = c('https://quarto-dev.r-universe.dev', 'https://cloud.r-project.org'))
# install.packages("terra", repos = "https://rspatial.r-universe.dev") # doesn't fail like CRAN
#library(Rcpp) # for terra
# # doesn't fail like CRAN

library(sf)
#library(raster)
library(leaflet)
library(tidyr)
library(dplyr)


#sf::sf_use_s2(FALSE)

path = "./data/"
#path = "./shiny/data/"

# WGS 84 version of datasets
nps_im <- st_read(paste0(path, "GIS/NPS_units_in_CGI_20250903_WGS84.shp"))
nps_im_df <- read.csv(paste0(path, "CGR_parks_prop_habitat.csv")) |> dplyr::select(-X, -Unit_Name, -zoom)

#nps_bbox1 <- st_bbox(nps_im1) * 1.01 # added 1% buffer
# nps_bbox <- data.frame(xmin = -113.16628, ymin = 29.38215,
#                        xmax = -95.35811, ymax = 47.85992)

nps_bbox <- data.frame(xmin = -118, ymin = 24,
                       xmax = -86, ymax = 49)

# long_mean = mean(c(nps_bbox$xmin, nps_bbox$xmax))
# lat_mean = mean(c(nps_bbox$ymin, nps_bbox$ymax))

networks <- st_read(paste0(path, "GIS/networks_WGS84.shp"))

#cent1 <- st_coordinates(st_centroid(st_as_sfc(nps_bbox)))
cent <- data.frame(long = -102, lat = 36.5)
nps_im2 <- nps_im |> dplyr::select(-UNIT_NA, -zoom)

names(nps_im_df)

network_list <- sort(unique(nps_im_df$Network))
park_list <- sort(unique(nps_im_df$Unit_Code))

# load other files
cgr_bound <- st_read(paste0(path, "GIS/Grasslands_Roadmap_boundary_Aug_2021_WGS84.shp"))

#cgr_shp <- st_read(paste0(path, "GIS/CGR_GAM_V2_WGS84_10km_simp.shp"))

# net_count <- nps_im_df |> filter(IM_Veg_Mon == 1) |> select(Unit_Code, Network, IM_Veg_Mon) |>
#   group_by(Network) |> summarize(num_parks = sum(!is.na(Unit_Code)))
