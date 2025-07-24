
# Starting from here
library(tidyverse)
library(sf)
library(raster)
#library(terra)
library(tmap)
options(scipen = 100)
sf_use_s2(FALSE)
nps_im <- st_read("./data/GIS/CGI_parks_network.shp")
nps_im$ID <- 1:nrow(nps_im)

# m2 to acres = m2/4046.863

cgr_ext_park <- raster("./data/GIS/CGR_GAM_V2_park_extract.tif")
dat <- levels(cgr_ext_park)[[1]]
dat$Category <- c("No Data", "Core Grassland", "Desert/Shrub",
                  "Vulnerable Grasslands",
                  "Converted/Altered Grasslands",
                  "Forest", "Developed", "Water")

levels(cgr_ext_park)[[1]] <- dat
#prod(res(cgr_ext_park)) #1278.99 m2

park_ext <- raster::extract(cgr_ext_park, nps_im, na.rm = T, df = T, exact = T)

park_ext_df <- as.data.frame(park_ext)

cats <- data.frame(ID = c(0, 5, 7, 100, 500, 1000, 2000, 5000),
                   Habitat = c("No Data", "Core Grassland", "Desert/Shrub",
                               "Vulnerable Grasslands",
                               "Converted/Altered Grasslands",
                               "Forest", "Developed", "Water"))

park_ext2 <- left_join(park_ext_df, nps_im, by = "ID")

park_ext3 <- full_join(park_ext2, cats, by = c("Category" = "ID"))

park_ext4 <- park_ext3 |> group_by(ID, Habitat, UNIT_CODE, NETCODE) |>
  summarize(num_cells = n(), .groups = 'drop') |>
  filter(!is.na(ID))

nps_im_df <- as.data.frame(st_drop_geometry(nps_im))
park_ext5 <- left_join(park_ext4, nps_im_df[,c("UNIT_CODE", "acres")], by = "UNIT_CODE") |> data.frame()

park_ext6 <- park_ext5 |> dplyr::select(-ID) |>
  pivot_wider(names_from = Habitat, values_from = num_cells, values_fill = 0) |>
  pivot_longer(cols = 4:10, names_to = "Habitat", values_to = "num_cells")

park_ext7 <- park_ext6 |> group_by(UNIT_CODE, NETCODE, acres) |>
  mutate(total_cells = sum(num_cells, na.rm = T),
         prop_hab = (num_cells/total_cells)*100,
         acres_hab = (prop_hab/100) * acres) |>
  arrange(UNIT_CODE, Habitat)

write.csv(park_ext7, "./data/CGR_parks_prop_habitat.csv")
#cgr_ext_park1km <- rast("./data/GIS/CGR_GAM_V2_park1km_extract.tif")
#cgr_ext_park10km <- rast("./data/GIS/CGR_GAM_V2_park10km_extract.tif")

