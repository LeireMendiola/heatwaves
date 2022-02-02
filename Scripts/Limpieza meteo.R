#Cleaning meteoland
library(tidyverse)


meteo <- read.csv("Data/Cleaned/meteo.csv")

Meteofiles  <- list.files()  

Meteotables <- lapply(Meteofiles, read.csv, header = TRUE,row.names=NULL)

Meteo18_1 <- Meteotables[[1]]#renaming each column of the dataset and removing some variables
colnames(Meteo18_1) <- c("Date","plot_id", "MeanTemperature", "MinTemperature","MaxTemperature","Precipitation","MeanRelativeHumidity", "MinRelativeHUmidity", "MaxRelativeHumidity", "Radiation", "WindSpeed", "WindDirection", "PET", "geometry", "geometry2") 

Meteo18_1 <- Meteo18_1 %>% select(-"geometry",-"geometry2")

Meteo18_1 <- na.omit(Meteo18_1) #same with all meteo files

Meteo18_1$plot_id<- str_remove(Meteo18_1$plot_id,"_")
Meteo18_1 <- Meteo18_1 %>% 
  rename(ID="plot_id") #to remove id and change nomenclature to ID

#2019#

Meteo19_1 <- Meteotables[[3]]#renaming each column of the dataset and removing some variables
colnames(Meteo19_1) <- c("Date","plot_id", "MeanTemperature", "MinTemperature","MaxTemperature","Precipitation","MeanRelativeHumidity", "MinRelativeHUmidity", "MaxRelativeHumidity", "Radiation", "WindSpeed", "WindDirection", "PET", "geometry", "geometry2") 

Meteo19_1 <- Meteo19_1 %>% select(-"geometry",-"geometry2", "WindDirection")

Meteo19_1 <- na.omit(Meteo19_1) #same with all meteo files

Meteo19_1$plot_id<- str_remove(Meteo19_1$plot_id,"_")
Meteo19_1 <- Meteo19_1 %>% 
  rename(ID="plot_id") #to remove id and change nomenclature to ID

colnames(Meteo19_2) <- c("Date","plot_id", "MeanTemperature", "MinTemperature","MaxTemperature","Precipitation","MeanRelativeHumidity", "MinRelativeHUmidity", "MaxRelativeHumidity", "Radiation", "WindSpeed", "WindDirection", "PET", "ThermalAmplitude", geometry", "geometry2") #este archivo tiene una variable mas que hay que quitar 

#despues de cargar todos los archivos los juntamos con rbind
write.csv(meteo,"C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/Cleaned")







