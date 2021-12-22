
### MODIS + ECOSTRESS ###

### En este script cogeremos datos de land surface temperature LST de ECOSTRESS, que tienen una resoluci?n espacial
## de 70x70 m pero temporalmente discretos, y los juntaremos con datos de LST MODIS, de resoluci?n de 1x1km 
## pero de periodicidad casi diaria debido a que son dos sat?lites (Terra y Aqua)
### Luego haciendo una sencilla transformaci?n, usaremos la relaci?n de la LST MODIS-ECOSTRESS
### para obtener datos casi diarios de LST a una resoluci?n 70x70m, para las parcelas del IFN de Catalunya

###abrimos paquetes 
install.packages("tidyverse")
library(tidyverse)### multipaquete data science
library(lubridate)      ### paquete gesti?n fechas y horas

setwd("~/Documents/BC3_msc/Probak/")    ### abrimos directorio donde est?n los archivos

files  <- list.files(pattern = 'ECO')  ###lee archivos ECOSTRESS con .csv de la carpeta #list.files produces a character vector of the names of files or directories in the named directory. pattern opens file names which match the regular expression.
tables <- lapply(files, read.csv, header = TRUE)  ### abre todos los archivos y los mete en una lista
eco <- do.call(rbind , tables)   ### junta todo los objetos de la lista en una sola tabla

class(eco$Date)  ###miramos de que clase (formato) es la columna Date

eco <- eco %>%
  mutate(Date_full=Date) %>%                 ###creamos nueva variable Date_full
  mutate(Date=ymd(as.Date(Date))) %>%       ### y ahora a Date le damos formato a?o-mes-d?a
  mutate(LST_eco=ECO2LSTE_001_SDS_LST)   %>%             ### nombre m?s corto para la LST
  filter(ECO2LSTE_001_SDS_QC_Mandatory_QA_flags=="0b00")%>%    ### filtramos s?lo datos buenos
  filter(!ECO2LSTE_001_SDS_LST==0) %>% #! indicates logical negation (NOT)
  filter(!ECO2LSTE_001_SDS_LST>3000)

files  <- list.files(pattern = '11A1')  ###lee todos los archivos MODIS de la carpeta en .csv
tables <- lapply(files, read.csv, header = TRUE,row.names=NULL)  ###abre todos los csv y los pone en una lista

modis<-dplyr::bind_rows(tables)   ###juntamos todas las tablas (mismo que antes, distinto metodo)

### Hay dos variables LST de modis, una por cada sat?lite, creamos la nueva variable LST_modis
###  que combina las dos, cuando modis$MOD11A1_006_LST_Day_1km es NA, se rellena con modis$MYD11A1_006_LST_Day_1km

modis$LST_modis = ifelse(is.na(modis$MOD11A1_006_LST_Day_1km),
                         modis$MYD11A1_006_LST_Day_1km,modis$MOD11A1_006_LST_Day_1km) #to conditionally fill values with another thing in this case with MYD if is.na = TRUE and with MOD if is.na = FALSE

### Y ahora ya filtramos, y nos quedamos s?lo con variables relevantes

modis<-modis %>%
  filter( MOD11A1_006_QC_Day_Data_Quality_flag == "0b00"| MYD11A1_006_QC_Day_Data_Quality_flag=="0b00")%>%
  filter(!LST_modis==0)%>%
  dplyr::select("ID","Date","Latitude","Longitude","LST_modis")%>% #creamos nueva tabla solo con esas variables
  mutate(Date=as.Date(Date)) #as.Date para cambiar fecha

### Ahora recortamos tambi?n los datos de ECOSTRESS

names(eco) ###esto nos da los nombres de las columnas

eco <- eco %>%
  mutate(hour=hour(Date_full)) %>%
  dplyr::select("ID","Date","Latitude","Longitude","Date_full","hour","LST_eco")

### Ahora juntams los datos MODIS y ECOSTRESS

modeco<-modis %>% full_join(eco,by=c("ID","Date","Latitude","Longitude"))

###As? queda 

head(modeco)        ###Vemos que hay m?s datos de LST_modis que de LST_eco. #por que?

library(broom.mixed)   ### abrimos un paquete para correr muchas regresiones simult?neamente
library(lme4)

###quitamos los casos que no tengan dato de LST_modis ya que no nos sirven de mucho

modeco0 <- modeco %>% mutate(ID=as.factor(ID)) %>% #lo de cambiarlo a factor podríamos haberlo hecho antés?
  filter(!LST_modis=="NA") 

### Calculamos la pendiente el intercepto de la relaci?n entre LST ecostress y LST MODIS

slopes<-modeco0 %>% 
  drop_na() %>% # to drop NA values
  group_by(ID) %>% 
  nest() %>% #creates a list column of data frames to summarize (you get one row for each group defined by the non-nested columns)
  mutate(model = map(data, ~ lm(LST_eco ~ LST_modis, data = .x) %>% 
                       tidy)) %>% 
  unnest(model) %>% 
  filter(term == 'LST_modis') 

intercept<-modeco0 %>% 
  drop_na() %>% 
  group_by(ID) %>% 
  nest() %>% 
  mutate(model = map(data, ~ lm(LST_eco ~ LST_modis, data = .x) %>% #lm to ft linear models. #.x is used to avoid name collisions of data sets
                       tidy)) %>% 
  unnest(model) %>% 
  filter(term == '(Intercept)')

### Escogemos s?lo esos casos d?nde la relaci?n entre LST sea significativa

slopes0 <- slopes %>%
  filter(p.value<0.05)%>%
  dplyr::select(ID,estimate)%>% 
  rename(slope=estimate)

intercept0 <- intercept %>%
  dplyr::select(ID,estimate) %>%
  rename(intercept=estimate)

### pegamos las slopes e interceptos con los datos, para poder crear la nuvea variable LST a 70x70m y casi diaria

modelled <- modeco0  %>%
  group_by("ID")%>%
  left_join(slopes0,by="ID")%>%
  left_join(intercept0,by="ID")%>%
  mutate(LST_plot=LST_modis*slope+intercept)%>%
  mutate(month=month(Date))%>%
  mutate(day=day(Date))%>%
  mutate(year=year(Date))%>%
  mutate(hour=hour(Date))

head(modelled)

### vamos a ver ahora los datos del plot P00001

### marcamos los d?as que fueron de ola de calor

modelled <- modelled %>% 
  mutate(heatwave = case_when(month == 7  & year == 2018 & day > 25 ~ "hw", 
                              month == 8  & year == 2018 & day < 7 ~ "hw", 
                              month == 8  & year == 2018 & day > 6 ~ "nonhw", 
                              month == 6  & year == 2019 & day > 24 ~ "hw", 
                              month == 7  & year == 2019 & day > 21 ~ "hw", 
                              month == 7  & year == 2019 & day > 25 ~ "nonhw", 
                              TRUE ~ "nonhw"))

### hacemos el plot

### para 50 parcelas CUIDADO PUEDE TARDAR UN POCO ###

### GUARDA LOS CAMBIOS

ggplot(modelled,aes(x=Date,y=LST_plot))+
  geom_point(aes(col=heatwave))+
  theme_bw()+
  theme(panel.grid = element_blank())

### para una parcela

ggplot(modelled %>% filter(ID=="P00001"),aes(x=Date,y=LST_plot))+
  geom_point(aes(col=heatwave))+
  theme_bw()+
  theme(panel.grid = element_blank())

library(ggpubr)  ###paquete para plotear regresiones

### vemos la relaci?n entre LST ecostress modis para una parcela en concreto

ggplot(modelled %>% filter(ID=="P00010"),aes(x=LST_modis,y=LST_eco))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_cor()

### datos de parcelas IFN

library(readxl)    ### paquete para abrir excels

nfidata<-read_excel("20201027_nfi_data.xlsx")
names(nfidata)   ###todas las variables de parcela

nfidata$plot_id<- str_remove(nfidata$plot_id,"_")   ###ponemos el mismo formato de ID del plot que en los datos sat?lite #str_remove replace the firs matched pattern in each string.
nfidata <- nfidata %>% rename(ID="plot_id") ###cambiamos m?nombre variable ID

### pegamos

modelled <-modelled %>%
  inner_join(nfidata,by=c("ID"))

### Ahora tenemos los datos de parcela, hacemos algunos gr?ficos r?pidos

###comparamos la LST entre las especies que tenemos

ggplot(modelled,aes(x=basal_area_species_dominant,y=LST_plot))+
  geom_boxplot(aes(fill=basal_area_species_dominant)) +
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_viridis_d()                        ###esto es la escala de color

### O el g?nero 

ggplot(modelled,aes(x=basal_area_genus_dominant,y=LST_plot))+
  geom_boxplot(aes(fill=basal_area_genus_dominant)) +
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_viridis_d()   

### O entre dias heatwave y normales pero s?lo de verano (fijate en el filtro)

summer <- modelled %>% filter (month>5 & month<9) #solo cogemos Junio, Julio y agosto?

ggplot(summer,aes(x=basal_area_species_dominant,y=LST_plot))+
  geom_boxplot(aes(fill=heatwave)) +
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_viridis_d()   

### Algunas relaciones cont?nuas

### con el leaf area index

ggplot(summer,aes(x=lai,y=LST_plot))+
  geom_point(aes(col=basal_area_genus_dominant)) +
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor()+
  theme(panel.grid = element_blank())+
  scale_color_viridis_d()   

### con la pluviometr?a anual

ggplot(summer,aes(x=clim_prec_year,y=LST_plot))+
  geom_point(aes(col=basal_area_genus_dominant)) +
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor()+
  theme(panel.grid = element_blank())+
  scale_color_viridis_d()   

### con la evapotranspiraci?n real

ggplot(summer,aes(x=clim_ret_year,y=LST_plot))+
  geom_point(aes(col=basal_area_genus_dominant)) +
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor()+
  theme(panel.grid = element_blank())+
  scale_color_viridis_d()   
