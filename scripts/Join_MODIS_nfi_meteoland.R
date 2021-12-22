#join MODIS with climatic data of NFI 

install.packages("readr")

setwd ("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/AquaTerraUnzipped") #for Mac use ("~/Users/...")

library(readr)

AquaTerrafiles  <- list.files(pattern = '11A1')  
AquaTerratables <- lapply(AquaTerrafiles, read.csv, header = TRUE,row.names=NULL)
modis <-dplyr::bind_rows(AquaTerratables)  

modis$LST_modis = ifelse(is.na(modis$MOD11A1_006_LST_Day_1km),
                         modis$MYD11A1_006_LST_Day_1km,modis$MOD11A1_006_LST_Day_1km) #to conditionally fill values with another thing in this case with MYD if is.na = TRUE and with MOD if is.na = FALSE
library(tidyverse)

modis<-modis %>%
  filter( MOD11A1_006_QC_Day_Data_Quality_flag == "0b00"| MYD11A1_006_QC_Day_Data_Quality_flag=="0b00")%>%
  filter(!LST_modis==0)%>%
  dplyr::select("ID","Date","Latitude","Longitude","LST_modis")%>% #creamos nueva tabla solo con esas variables
  mutate(Date=as.Date(Date)) #as.Date para cambiar formato fecha

#para ordenar datos por fecha 
  modis <- modis %>% arrange(ID,Date)
 
#more space between command subgroups!

#two LST data in some cases (Terra and Aqua), we will add the mean in each observation
modis <- aggregate(modis$LST_modis, by=list(ID=modis$ID,Date=modis$Date, Latitude=modis$Latitude, Longitude=modis$Longitude), data=modis, FUN=mean)

library(lubridate) 
 
modis <- modis %>% 
   mutate(month=month(Date))%>%
   mutate(day=day(Date))%>%
   mutate(year=year(Date))

write.csv(modis,"C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/modis.csv") #for saving df in my directory. If i am in the actual directory ./

#We load NFI data 

setwd ("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/parcela")

nfi_data <- read.csv2("20201027_nfi_data.csv") #bind rows and lapply are not necessary because we only have one  

names(nfi_data) #to see all variables (211)

nfi_climatic <- nfi_data %>% dplyr::select(plot_id,clim_tmax_may,clim_tmax_jun,clim_tmax_jul,clim_tmax_aug,clim_tmax_sep,clim_tmin_may,clim_tmin_jun,clim_tmin_jul,clim_tmin_aug,clim_tmin_sep) #for selecting climate variables of nfi

nfi_climatic$plot_id<- str_remove(nfi_climatic$plot_id,"_") #to homogenate ID nomenclature

nfi_climatic <- nfi_climatic %>% rename(ID="plot_id") #changing variable name from plot_id to ID

write.csv(nfi_climatic,"C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/Cleaned/NFI.csv") #guardamos datos climaticos parcela antes de pegarlos a modis y meteo

nfi_modis <-modis %>%
  inner_join(nfi_climatic,by=c("ID")) 

nfi_modis <- nfi_modis %>% select(-month, -day,-year) #dont do this because then filtering is more difficult

#juntar con todos los datos de Meteoland 

# 1. llamamos al archivo NFI_meteoland ya unido:

setwd("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/Cleaned")

library(readr)
modisNFI <- read_csv("modisNFI")

setwd ("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/Meteoland")

Meteofiles  <- list.files()  

Meteotables <- lapply(Meteofiles, read.csv, header = TRUE,row.names=NULL)

Meteotables1 <- Meteotables[[1]]

Meteotables1 <- as.data.frame(Meteotables1)
colnames(Meteotables1) <- c("Date","plot_id", "MeanTemperature", "MinTemperature","MaxTemperature","Precipitation","MeanRelativeHumidity", "MinRelativeHUmidity", "MaxRelativeHumidity", "Radiation", "WindSpeed", "WindDirection", "PET", "geometry", "geometry2") 
#para sustituir los nombres de las variables, lo mismo para cada archivo antes de juntarlos 
Meteotables1 <- select(Meteotables1, -geometry,-geometry2,-WindDirection)

#do this for all the tables in the list and then join in one df

meteo <-dplyr::bind_rows(Meteotable1,...)

write.csv(meteo,"C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/Cleaned/meteo.csv")

#para juntar con los datos de Modis y NFI, la nomenclatura ID tiene que ser uniforme

meteo$plot_id<- str_remove(meteo$plot_id,"_")
meteo <- meteo %>% 
  rename(ID="plot_id")

#cargamos datos NFImodis y los juntamos con meteo seg?n ID 

meteo_modis <-modis %>%
  left_join(meteo,by=c("ID", "Date")) 

#vctrs cant bind <character> and <double> one must change

df1 <- df1 %>% mutate(b = as.character(b))

#Guardar dataframe unido y empezamos con la descripci?n.

#amplitude variable needs to be remove from [[4]] Meteoland .csv
