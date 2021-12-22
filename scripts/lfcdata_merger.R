
### Extract data from Laboratori Forestal Català

remotes::install_github('MalditoBarbudo/lfcdata', ref = 'master', force = TRUE)#paketea github kontu batetik instalatzeko 

library(lfcdata)

###
library(udunits2)
library(lfcdata)
if (!require('sp')) install.packages('sp'); library('sp')
if (!require('sf')) install.packages('sf'); library('sf')

### a veces hay que reinstalar el udunits2

meteolanddb <- meteoland()
nfidb <- nfi()
catdroughtdb <-catdrought() 

sf_points<- nfidb %>%
  nfi_get_data('plots', spatial = TRUE) %>%
  dplyr::select(plot_id) %>%
  slice(1:5)

sf_points<-arrange(sf_points, plot_id)

meteo_test<-lfcdata::meteoland_points_interpolation(
  meteolanddb,
  sf=sf_points, 
  dates=c("2018-08-01","2021-08-31"),
  points_id = "plot_id")

rew_test<-lfcdata::catdrought_get_current_time_series(
  catdroughtdb,
  sf=sf_points, 
  variable="REW")

Eplant_test<-lfcdata::catdrought_get_current_time_series(
  catdroughtdb,
  sf=sf_points, 
  variable="Eplant",
  resolution="smoothed")

sf_points$plot_id<- str_remove(sf_points$plot_id,"_")   ###ponemos el mismo formato de ID del plot que en los datos satélite
sf_points<- sf_points %>% rename(ID="plot_id") ###cambiamos m¡nombre variable ID

meteo_test$plot_id<- str_remove(meteo_test$plot_id,"_")   ###ponemos el mismo formato de ID del plot que en los datos satélite
meteo_test <- meteo_test %>% rename(ID="plot_id")  

rew_test$point_id<- str_remove(rew_test$point_id,"_")   ###ponemos el mismo formato de ID del plot que en los datos satélite
rew_test <- rew_test %>% rename(ID="point_id") %>% rename(date="day") ###cambiamos m¡nombre variable ID

Eplant_test$point_id<- str_remove(Eplant_test$point_id,"_")   ###ponemos el mismo formato de ID del plot que en los datos satélite
Eplant_test <- Eplant_test %>% rename(ID="point_id") %>% rename(date="day")###cambiamos m¡nombre variable ID

ggplot(rew_test,aes(x=day,y=REW))+
  geom_point()+
  facet_wrap(~point_id)+
  theme_bw()

####
#### Ahora lo pegamos todo a modelled que viende del join_modis_ecostress.R

modelled_full <- modelled %>% 
  rename(date="Date") %>% 
  mutate(date=as.Date(date)) %>% 
  left_join(meteo_test,by=c("ID","date")) %>%
  left_join(rew_test,by=c("ID","date")) %>%
  left_join(Eplant_test,by=c("ID","date")) 

ggplot(modelled_full %>% filter (month>6 & month<9),aes(x=Eplant,y=REW))+
  geom_point(aes(col=basal_area_species_dominant))+
  stat_cor()
