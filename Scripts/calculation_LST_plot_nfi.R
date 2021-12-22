###
### Join together MODIS and ECOSTRESS, estimate LST_eco
###
library(tidyverse)
library(readxl)

setwd('C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/AppEEARS/Ecostress/Eco_LST') 

setwd("/Volumes/Leire_BC3/Data/inputs/AppEEARS/Ecostress/Eco_LST_18_20")

files  <- list.files(pattern = '\\.csv')  ###read all ECOSTRESS .csv in folder
tables <- lapply(files, read.csv, header = TRUE)  ###open them and put them in a list

lst_eco <- bind_rows(tables)   ### paste together all tables

lst_eco<-lst_eco %>% mutate(lst_eco,plot_id=ID)  ### rename plot variable

lst_eco<-filter(lst_eco,ECO2LSTE_001_SDS_QC_Mandatory_QA_flags=="0b00")   ### Only good measurements

head(lst_eco)                   ### check how it looks
hist(lst_eco$ECO2LSTE_001_SDS_LST)
names(lst_eco)

lst_eco$month<-lubridate::month(lst_eco$Date)
lst_eco$hour<-lubridate::hour(lst_eco$Date)

lst_eco_summer<-lst_eco %>% filter(month=="5"|month=="6"|month=="7"|month=="8"|month=="9")  ### Only growing season months

### Take only hours in the middle of the day
### UTC = CET+2 so, if we choose between 9 and 17 UTC we are picking between 11 and 19

lst_eco_summer_day<-lst_eco_summer %>% filter(hour>9&hour<16)

### Paste MODIS

modis <- read_csv("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/Cleaned/modis_LSTmean.csv") ### File from Leire
modis <-read_csv("/Volumes/Leire_BC3/Data/inputs/Cleaned/modis_LSTmean.csv")
modis <- modis %>% mutate(Date=as.Date(Date)) 

### Data should be clean but:

lst_eco_summer_day <- lst_eco_summer_day %>% 
  mutate(Date=as.Date(Date)) %>%
  mutate(month=lubridate::month(Date)) %>%
  mutate(Date_time=Date) %>%
  dplyr::select(c("ID","Date","month","hour","ECO2LSTE_001_SDS_LST")) %>%
  dplyr::rename(LST_eco=ECO2LSTE_001_SDS_LST) 

modeco <- modis %>%
  left_join(lst_eco_summer_day,by=c("ID","Date")) %>%
  dplyr::select(-c("...1"))

head(modeco)

str(modeco)

modeco <- modeco %>% mutate(ID=as.factor(ID))

summary(model<-lm(LST_modis~LST_eco,modeco))       ### check the overall relationship

library(ggplot2)
ggplot(modeco, aes(LST_eco,LST_modis)) +
  geom_point() +
  geom_smooth(method="lm", level = 0.8)
  

library(broom)
library(dplyr)
library(tidyr)
library(purrr)

poslm <- possibly(lm, otherwise = NA)

models <- modeco %>%
  group_by(ID) %>% 
  nest() %>%
  mutate(fit = map(data, ~ poslm(LST_eco ~ LST_modis, data = .x)), 
         tidied = map(fit, possibly(tidy, otherwise = NA)),
         augmented = map(fit, possibly(augment, otherwise = NA)),
         glanced = map(fit, possibly(glance, otherwise = NA)))              ### run models LST_eco ~ LST_modis

### Extract slopes (only significant ones)

slopes <- models %>%
  unnest(tidied) %>%
  filter(term == 'LST_modis') %>%
  filter(p.value < 0.05 & estimate > 0) %>%
  dplyr::select(c("ID","term","estimate","std.error","statistic","p.value")) %>%
  dplyr::rename(slope=estimate)

### Extract intercepts

intercept<-models %>%
  unnest(tidied) %>%
  filter(term == '(Intercept)') %>%
  dplyr::select(c("ID","term","estimate","std.error","statistic","p.value")) %>%
  dplyr::rename(intercept=estimate) 

### Extract r-squareds

rsq <- models %>%
  unnest(glanced) %>%
  dplyr::select(c("ID","r.squared","adj.r.squared","sigma","statistic","df.residual","p.value"))

### join together with data, at the plot (ID) level

modelled_lst <- modeco  %>%
  group_by(ID) %>%
  full_join(slopes,by="ID")%>%
  full_join(intercept,by="ID")%>%
  left_join(rsq,by="ID")%>%
  mutate(LST_plot=LST_modis*slope+intercept)%>%     ### this line calculates LST_plot
  filter(!LST_plot=="NA") %>%
  filter(LST_plot >= 273) %>%
  filter(r.squared>0.6999999)

### Now you can paste it with meteoland data, NFI data, LIDAR data and medfate data