#measuring HW severity 
library(lubridate)
library(tidyverse)

#cargamos datos completos desde 21-05-01 al 21-08-31  

data <- read.csv("Data/Cleaned/HeatFlux_21.csv")

#Calculate the Tmax of HW days (11/08-15/08, both included)

data <- data %>%
   mutate(heatwave = 
         case_when(Date >= ymd("2021-08-11") & Date < ymd("2021-08-16") ~ "hw",
                   TRUE ~ "nonhw")) #why doesnt filter

HW_data <- data %>%
  filter(heatwave == "hw")%>%

HW_mean_Tmax <- HW_data %>%
  group_by(ID) %>%
  summarise(mean_Tmax=mean(MaxTemperature, na.rm=T))

#paste mean_tmax data to HW data

HW_data <- HW_data %>%
  left_join(HW_mean_Tmax, by= "ID")
  
#Calculate the difference between HW mean Tmax and Tmax of august

hw_data <- HW_data %>%
  mutate(severity = mean_Tmax - clim_tmax_aug)

#map the results!
