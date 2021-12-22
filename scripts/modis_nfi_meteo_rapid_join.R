#pegar archivos modis_meteo_nfi

setwd ("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/Cleaned")
library(readr)

#cargamos modis
modis <- read_csv("modis_LSTmean.csv")

#cargamos meteo
meteo <- read_csv("meteo.csv")

library(tidyverse)
meteo$plot_id<- str_remove(meteo$plot_id,"_")
meteo <- meteo %>% 
  rename(ID="plot_id") #unificamos ID para poder juntarlo, hacemos lo mismo con el archivo meteo21

meteo_2021 <- read.csv("nfi_meteoland_2021_1.csv")
df1 <- df1 %>% mutate(b = as.character(b))

names(meteo[[1]]) <- names(nfi_meteoland_2021_1[[2]]) 
identical(names(meteo[[1]]), names(nfi_meteoland_2021_1[[2]]) )
[1] TRUE


meteo_joined <- rbind()
#Juntamos
meteo_modis <-meteo %>%
  left_join(modis,by=c("ID", "Date")) 

he#cargamos nfi
nfi <- read_csv("NFI.csv")

meteo_modis_nfi <- meteo_modis %>% full_join(nfi, by=c("ID"))

write.csv(meteo_modis_nfi,"C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/Cleaned/meteo_modis_nfi.csv")
