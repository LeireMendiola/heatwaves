#full dataset join

library(readr)
library(readxl)
library(tidyverse)
library(lubridate)

#cargamos datos LST_plot obtenidos con el downscaling Modis-Ecostress
LST_plot <- read_csv("Data/Cleaned/modis_LSTmean.csv")

#cargamos datos NFI y comprobamos estructura
nfi <- readxl::read_excel("Data/parcela/20201027_nfi_data.xlsx") 

str(nfi)

nfi <- nfi %>% dplyr::rename(ID = plot_id)
nfi$ID <- nfi$ID %>% str_replace('_', '')

lst_nfi <- left_join(LST_plot,nfi, by="ID")

#cargamos datos de transpiración del modelo medfate

plant <- read.csv('Data/Cleaned/e_plant_2021.csv') #son datos del 11/07/20 al 11/07/21
plant <- plant %>%
  dplyr::mutate(Date = lubridate::ymd(day)) %>% 
  dplyr::rename(ID = point_id) %>% 
  dplyr::select(-c(X, day)) 
plant$ID <- plant$ID %>% str_replace("_", "")

lst_nfi_eplant <- left_join(lst_nfi,plant, by = c("ID", "Date"))

#cargamos datos de relative extractable water de medfate

rew <- read.csv("Data/Cleaned/rew_2021.csv") #son datos del 11/08/20 al 11/08/21

rew <- rew %>%
  dplyr::mutate(Date = lubridate::ymd(day)) %>% 
  dplyr::rename(ID = point_id) %>% 
  dplyr::select(-c(X, day)) 
rew$ID <- plant$ID %>% str_replace("_", "")

lst_nfi_eplant_rew <- left_join(lst_nfi_eplant,rew, by = c("ID", "Date"))

#cargamos datos meteorologicos de meteoland
meteo <- read_csv("Data/Cleaned/meteoland_with_2021_correct.csv")

meteo$ID<- str_remove(meteo$ID,"_")
meteo <- meteo %>% rename("Date" = "Date2")

#meteoland df is to big to bind so we will first filter it according to lst_plot ID levels

lst_ID <- as.data.frame(levels(as.factor(LST_plot$ID))) #he creado un vector sin variable id. as.data.frame to create a dataframe from that vector 
colnames(lst_ID) <- "ID"

meteo_clean <- lst_ID %>% 
  left_join(meteo, by=c("ID"))

lst_nfi_eplant_rew_meteo <-lst_nfi_eplant_rew %>%
  left_join(meteo_clean,by=c("ID", "Date")) 

#cargamos lidar

lidar <- read_csv("Data/Cleaned/Lidar.csv")

lidar$ID<- str_remove(lidar$ID,"_")

lst_nfi_eplant_rew_meteo_lidar <- lst_nfi_eplant_rew_meteo %>% left_join(lidar, by = "ID")

head(subset(lst_nfi_eplant_rew_meteo_lidar, ID == "P00003")) #for subsetting data 

write.csv(lst_nfi_eplant_rew_meteo_lidar,"Data/Cleaned/lst_nfi_Eplant_rew_meteo_lidar.csv")
