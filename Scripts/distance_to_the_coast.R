devtools::install_github("mdsumner/distancetocoast")

library(udunits2)
library(distancetocoast)
library(raster)
library(sp)
library(tidyverse)
library(readxl)
library(terra)

nfidata <- read.csv2("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/parcela/20201027_nfi_data.csv",row.names = NULL)

nfi_coords <- nfidata %>%
  dplyr::select(c("plot_id","coords_latitude","coords_longitude","admin_region"))

nfi_coords_short <- nfidata %>%
  dplyr::select(c("coords_longitude","coords_latitude")) %>%
  mutate(longitude=as.numeric(coords_longitude)) %>%
  mutate(latitude=as.numeric(coords_latitude)) %>%
  dplyr::select(-c(1:2))

plot(distance_to_coastline_lowres, col = viridis::viridis(64))
plot(rnaturalearth::ne_coastline(), add = TRUE) #when instaling rnaturalearthdata which item of the menu do I have to choose?

ex <- extent(0, 4, 40, 43)
plot(crop(distance_to_coastline_10, ex), col = viridis::viridis(64))
plot(rnaturalearth::ne_coastline(10), add = TRUE)

raster::extract(distance_to_coastline_10, nfi_coords_short)

distances <- raster::extract(distance_to_coastline_lowres, nfi_coords_short)

distances_nfi <- as.data.frame(bind_cols(nfidata$plot_id,distances))
colnames(distances_nfi) <- c("plot_id","distance_to_sea_m")

#write.csv(distances_nfi,"D:/Documents/Heatwaves_Eco/distances_nfi.csv")

### From meteocat

station_data <- readxl::read_xlsx("D:/MeteoCat/XEMA_2006-05-01_2021-09-30.xlsx")

head(station_data)

library(terra)

points <- station_data %>% dplyr::select(c("XUTM","YUTM"))

v <- vect(cbind(points$XUTM,points$YUTM), crs="+proj=utm +zone=31 +datum=WGS84  +units=m")
v
y <- project(v, "+proj=longlat +datum=WGS84")
y
lonlat <- geom(y)[, c("x", "y")]
head(lonlat, 3)

meteo_dist<-as.data.frame(raster::extract(distance_to_coastline_10, lonlat))

colnames(meteo_dist)<-"Dist_sea"

station_data <- bind_cols(station_data,meteo_dist) 
  
