###
###
###     Station data MeteoCat
###
###
###
library(tidyverse)
library(readxl)
library(roll)
library(lubridate)
library(factoextra)

setwd("D:/MeteoCat")

meteocat <- readxl::read_excel("Aa_13897.xlsx")

head(meteocat)

summer_tmax<-meteocat %>% 
  mutate(Date=as.Date(DATA)) %>%
  mutate(Year=year(Date)) %>%
  mutate(doy=yday(Date)) %>%
  mutate(month=month(Date)) %>%
  filter(month>5 & month < 9) %>%
  group_by(EMA) %>%
  summarise(q95_TX=quantile(TX,0.95,na.rm=T),q95_TM=quantile(TM,0.95,na.rm=T),q95_TN=quantile(TN,0.95,na.rm=T)) 

head(summer_tmax)

meteocat_q95 <- meteocat %>%
  mutate(Date=as.Date(DATA)) %>%
  mutate(Year=year(Date)) %>%
  mutate(doy=yday(Date)) %>%
  mutate(month=month(Date)) %>%
  left_join(summer_tmax,by=c("EMA")) 

meteocat_q95$heatwave[meteocat_q95$q95_TX>=meteocat_q95$TX] <- "non-heatwave" 
meteocat_q95$heatwave[meteocat_q95$q95_TX<meteocat_q95$TX] <- "heatwave" 

meteocat_q95 %>% count(heatwave)

### Add site data for stations

station_data <- readxl::read_xlsx("XEMA_2006-05-01_2021-09-30.xlsx")

station_data <- station_data %>%
  dplyr::rename(EMA=Codi)

### Paste distance to the sea from the script "distance_to_the_coast"

meteocat_1821 <- meteocat_q95 %>%
  left_join(station_data,by="EMA") %>%
  filter(Year>2017) %>%
  mutate(TX_diff=TX-q95_TX)

# Periods -----------------------------------------------------------------

library(dplyr)

summer_hw <- meteocat_q95 %>%
  arrange(EMA,Date) %>%
  group_by(heatwave,EMA) %>%
  filter(month>5 & month<9)
  
summer_hw <- summer_hw %>%
  group_by(EMA, grp = with(rle(heatwave), rep(seq_along(lengths), lengths))) %>%
  mutate(Consecutive = seq_along(grp)) %>%
  ungroup() %>%
  dplyr::select(-grp)

only_hw <- summer_hw %>%
  filter(Year> 2017) %>%
  filter(heatwave=="heatwave") %>%
  mutate(TX_diff = TX-q95_TX) %>%
  left_join(station_data,by="EMA") 

## Ckeck distribution and severity

ggplot(only_hw,aes(x=doy,y=TX_diff))+
  geom_jitter()+
  facet_wrap(~Year)+
  theme_bw()

### Subset by events

event2018 <- only_hw %>%
  filter(doy > 210 & doy < 220 & Year == 2018)

event2019_june <- only_hw %>%
  filter(doy > 169 & doy < 186 & Year == 2019)

event2019_july <- only_hw %>%
  filter(doy > 199 & doy < 211 & Year == 2019)

event2020_july <- only_hw %>%
  filter(doy > 208 & doy < 218 & Year == 2020)

event2020_aug <- only_hw %>%
  filter(doy > 218 & doy < 227 & Year == 2020)

event2021_july <- only_hw %>%
  filter(doy > 198 & doy < 210 & Year == 2021)

event2021_aug <- only_hw %>%
  filter(doy > 218 & doy < 232 & Year == 2021)

# PCA ---------------------------------------------------------------------

### PCA by events

### 2018

event2018_pca <- event2018 %>%
  dplyr::select(c("TX_diff","Altitud","XUTM","YUTM","Dist_sea")) 

event2019_june.pca <- prcomp(event2018_pca, scale = T)
get_pca_var(event2019_june.pca)$cos2
fviz_eig(event2019_june.pca)

fviz_pca_var(event2019_june.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)

### 2019

event2019_june_pca <- event2019_june %>%
  dplyr::select(c("TX_diff","Altitud","XUTM","YUTM","Dist_sea")) 

event2019_june.pca <- prcomp(event2019_june_pca, scale = T)
get_pca_var(event2019_june.pca)$cos2
fviz_eig(event2019_june.pca)

fviz_pca_var(event2019_june.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)

event2019_july_pca <- event2019_july %>%
  dplyr::select(c("TX_diff","Altitud","XUTM","YUTM","Dist_sea")) 

event2019_july.pca <- prcomp(event2019_july_pca, scale = T)
get_pca_var(event2019_july.pca)$cos2
fviz_eig(event2019_july.pca)

fviz_pca_var(event2019_july.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)

### 2020

event2020_aug_pca <- event2020_aug %>%
  dplyr::select(c("TX_diff","Altitud","XUTM","YUTM","Dist_sea")) 

event2020_aug.pca <- prcomp(event2020_aug_pca, scale = T)
get_pca_var(event2020_aug.pca)$cos2
fviz_eig(event2020_aug.pca)

fviz_pca_var(event2020_aug.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)

event2020_july_pca <- event2020_july %>%
  dplyr::select(c("TX_diff","Altitud","XUTM","YUTM","Dist_sea")) 

event2020_july.pca <- prcomp(event2020_july_pca, scale = T)
get_pca_var(event2020_july.pca)$cos2
fviz_eig(event2020_july.pca)

fviz_pca_var(event2020_july.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)

### 2021

event2021_aug_pca <- event2021_aug %>%
  dplyr::select(c("TX_diff","Altitud","XUTM","YUTM","Dist_sea")) 

event2021_aug.pca <- prcomp(event2021_aug_pca, scale = T)
get_pca_var(event2021_aug.pca)$cos2
fviz_eig(event2021_aug.pca)

fviz_pca_var(event2021_aug.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)

event2021_july_pca <- event2021_july %>%
  dplyr::select(c("TX_diff","Altitud","XUTM","YUTM","Dist_sea")) 

event2021_july.pca <- prcomp(event2021_july_pca, scale = T)
get_pca_var(event2021_july.pca)$cos2
fviz_eig(event2021_july.pca)

fviz_pca_var(event2021_july.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)




