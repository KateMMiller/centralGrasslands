
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

park_ext <- raster::extract(cgr_ext_park, nps_im, na.rm = T, df = T, exact = T)
park_ext_df <- as.data.frame(park_ext)

cats <- data.frame(ID = c(0, 5, 7, 100, 500, 1000, 2000, 5000),
                   Habitat = c("No Data", "Core Grassland", "Desert/Shrub",
                               "Vulnerable Grasslands",
                               "Converted/Altered Grasslands",
                               "Forest", "Developed", "Water"))

park_ext2 <- left_join(park_ext_df, nps_im, by = "ID")
head(park_ext2)
park_ext3 <- left_join(park_ext2, cats, by = c("Category" = "ID"))
head(park_ext3)

park_ext4 <- park_ext3 |> group_by(ID, Habitat, UNIT_CODE, NETCODE) |>
  summarize(num_cells = n())

park_ext4

#cgr_ext_park1km <- rast("./data/GIS/CGR_GAM_V2_park1km_extract.tif")
#cgr_ext_park10km <- rast("./data/GIS/CGR_GAM_V2_park10km_extract.tif")

nps_im$ID <- 1:nrow(nps_im)



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


