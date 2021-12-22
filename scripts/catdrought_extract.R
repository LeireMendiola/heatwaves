
.libPaths()

.libPaths("C:/R-4.0.3/library")

remotes::install_github('MalditoBarbudo/lfcdata', ref = 'catdrought',force=TRUE)

# install.packages("remotes")
remotes::install_github("MalditoBarbudo/lfcdata", ref = "staging", build_vignettes = FALSE)

library(lfcdata)
library(tidyverse)
library(catdrought)
library(lubridate)

catddb<-catdrought()

nfidb <- nfi()
nfidb

sf_points<-nfidb %>%
  nfi_get_data('plots', spatial = TRUE) %>%
  select(plot_id) 

###creem objecte espacial m?s petit per fer tests
library(sf)

sf_points_1 <- nfidb %>% 
  nfi_get_data('plots', spatial = TRUE) %>%
  select(plot_id) %>%
  slice(1:5)

e_plant<-catdrought_get_current_time_series(catddb,sf_points,variable=c("Eplant"))
write.csv(e_plant,"D:/CATDROUGHT2021/e_plant_2021.csv")

cat_theta<-catdrought_get_current_time_series(catddb,sf_points,variable=c("Theta"))
write.csv(cat_theta,"D:/CATDROUGHT2021/cat_theta_2021.csv")

e_soil<-catdrought_get_current_time_series(catddb,sf_points,variable=c("Esoil")) #

LAI_catdrought<-catdrought_get_current_time_series(catddb,sf_points,variable=c("LAI"))

rew<-catdrought_get_current_time_series(catddb,sf_points,variable=c("REW"))

Psi<-catdrought_get_current_time_series(catddb,sf_points,variable=c("Psi"))

