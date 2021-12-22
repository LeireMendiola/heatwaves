###
### Script cálculos olas de calor
###
library(tidyverse)
library(lubridate)
### abro mi archivo donde lo tengo todo

datuak <- read.csv("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/Cleaned/meteo_modis_nfi.csv")
### Cogemos sólo unas parcelas para ir más rápido

random_data <- datuak %>% 
  filter(ID=="P05344"|ID=="P04186"|ID=="P00453"|ID=="P08117"|ID=="P04900"|ID=="P04004")
#orain probak egiteko NFImodis-ekin

head(random_data)

### Abrimos datos nfi

nfidata <- readxl::read_excel("D:/Documents/ECOSTRESS/IFN CAT/20201027_nfi_data.xlsx")

random_nfi <- nfidata %>%
  mutate(ID=str_remove(plot_id,"_")) %>%  #para homogenizar ID
  filter(ID=="P05344"|ID=="P04186"|ID=="P00453"|ID=="P08117"|ID=="P04900"|ID=="P04004") %>%
  dplyr::select(c("ID","month", "clim_tmax_jun","clim_tmax_jul","clim_tmax_aug")) #tengo ya los datos entre mayo y septiembre

## Pegamos    #esto no lo corro porque ya tengo los archivos meteo + modis + nfi

random_data <- random_data %>%
  left_join(random_nfi,by="ID")

names(random_data)

### Creamos variable month

random_data <- random_data %>%
  mutate(month=month(Date))

### Calculamos LST media por meses, y desviación

setwd("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/Cleaned") #prueba, luego cargar todo desde el archivo conjunto de modis, meteo y NFI

modis_NFI <- read_csv("modis_NFI") #Para abrir directamente datos modis y climaticos de junio, julio y agosto

random_data <- random_data %>%
  group_by(ID,month) %>%
  mutate(LST_monthly_mean=mean(LST_plot,na.rm=T))  %>%
  mutate(LST_z=(LST_plot-LST_monthly_mean)/sd(LST_plot,na.rm=T)) 

### Calculamos desviación de la Air T

random_data <- random_data %>%
  group_by(ID) %>%
  mutate(AirT_anomaly = case_when(
    month=="6" ~ MaxTemperature-clim_tmax_jun,
    month=="7" ~ MaxTemperature-clim_tmax_jul,
    month=="8" ~ MaxTemperature-clim_tmax_aug)) %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(heatwave = 
           case_when(Date >= ymd("2018-08-01") & Date < ymd("2018-08-07") ~ "hw",
                     Date >= ymd("2019-06-25") & Date < ymd("2019-07-01") ~ "hw",
                     Date >= ymd("2019-07-22") & Date < ymd("2019-07-26") ~ "hw",
                     Date >= ymd("2020-07-30") & Date < ymd("2020-08-02") ~ "hw",
                     TRUE ~ "nonhw")) %>%
  mutate(heatwave_end = 
           case_when(Date == ymd("2018-08-06") | Date == ymd("2018-08-05") | Date == ymd("2018-08-04") ~ "hw_end",
                     Date == ymd("2019-06-30") | Date == ymd("2019-06-29") | Date == ymd("2019-06-28") ~ "hw_end",
                     Date == ymd("2019-07-25") | Date == ymd("2019-07-24") | Date == ymd("2019-08-01") ~ "hw_end",
                     Date == ymd("2020-08-01") ~ "hw_end",
                     TRUE ~ "nonhw")) %>%
  mutate(year=year(Date)) 

### Comparamos heatwaves con el resto

ggplot(random_data,aes(x=heatwave,y=AirT_anomaly))+
  geom_boxplot()+
  theme_bw()+
  theme(panel.background = element_blank())

ggplot(random_data,aes(x=heatwave,y=AirT_anomaly))+
  geom_boxplot()+
  theme_bw()+
  theme(panel.background = element_blank())


### Comparamos años y heatwaves

ggplot(random_data,aes(x=heatwave,y=LST_z))+
  geom_boxplot()+
  theme_bw()+
  facet_wrap(~year)+
  theme(panel.background = element_blank())

ggplot(random_data,aes(x=heatwave,y=AirT_anomaly))+
  geom_boxplot()+
  theme_bw()+
  facet_wrap(~year)+
  theme(panel.background = element_blank())

### Comparamos dentro de heatwave, hacia al final de la ola y al principio

ggplot(random_data %>% filter (heatwave=="hw"),aes(x=heatwave_end,y=LST_z))+
  geom_boxplot()+
  theme_bw()+
  facet_wrap(~year)+
  theme(panel.background = element_blank())

ggplot(random_data %>% filter (heatwave=="hw"),aes(x=heatwave_end,y=AirT_anomaly))+
  geom_boxplot()+
  theme_bw()+
  facet_wrap(~year)+
  theme(panel.background = element_blank())

### relacion entre anomalías

ggplot(random_data,aes(x=scale(AirT_anomaly),y=LST_z))+
  geom_point()+
  geom_smooth()+
  ggpubr::stat_cor()+
  theme(panel.background = element_blank())+
  theme_bw()+
  geom_abline(slope=1,intercept=1)
