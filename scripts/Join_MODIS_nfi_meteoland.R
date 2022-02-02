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

#to get mean of repeated LST observations with terra and aqua 
modis <- aggregate(modis$LST_modis,by=list(ID=modis$ID,Date=modis$Date,Latitude=modis$Latitude,Longitude=modis$Longitude),data=modis,FUN=mean)

#para ordenar datos por fecha 
  modis <- modis %>% arrange(ID,Date)
 

summary(modis) 

library(lubridate) 
 modis <- modis %>% 
   mutate(month=month(Date))%>%
   mutate(day=day(Date))%>%
   mutate(year=year(Date))

write.csv(nombredelatable,"C:/Users/User/LEIRE/heatwaves/heatwaves/Data/outputs.csv") #for saving df in my directory. If i am in the actual directory ./

setwd ("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/parcela")
nfi_data <- read_delim("20201027_nfi_data.csv", 
                                 +     delim = ";", escape_double = FALSE, trim_ws = TRUE) #for reading .csv separated by ";"
names(nfi_data) #to see all variables (211)

nfi_climatic <- nfi_data %>% dplyr::select(plot_id, starts_with("clim")) #for selecting climate variables of nfi

names(nfi_climatic)

nfi_climatic$plot_id<- str_remove(nfi_climatic$plot_id,"_") #to homogenate ID nomenclature

nfi_climatic <- nfi_climatic %>% rename(ID="plot_id") #changing variable name from plot_id to ID

nfi_modis <-modis %>%
  inner_join(nfi_climatic,by=c("ID"))

#juntar con todos los datos de Meteoland 

Meteofiles  <- list.files()  
Meteotables <- lapply(Meteofiles, read.csv, header = TRUE,row.names=NULL)
Meteotable <- do.call(rbind , Meteotables)
#amplitude variable needs to be remove from [[4]] Meteoland .csv
