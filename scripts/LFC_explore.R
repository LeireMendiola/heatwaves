
#para explorar datos LFC
setwd ("C:/Users/User/LEIRE/heatwaves/heatwaves/Data/inputs/LFC")
e_plant <- read.csv("e_plant_2021.csv")
head(e_plant)


#probamos para unas pocas parcelas
random_data <- e_plant %>% 
  filter(point_id=="P_05344") %>%
  select("day","ID", "Eplant")


library(tidyverse)
library(lubridate)
random_data <- random_data %>%
  mutate(ID=str_remove(point_id,"_"))
#creamos variable mes
random_data <- random_data %>%
  mutate(month=month(Date))

dates <- random_data$day
dates <- data.frame(as.Date(dates, format = "%Y-%m-%d"))
random_data$day <- dates   #error: x do not know how to convert 'x' to class "POSIXlt"



random_data <- random_data %>%
  select("day","ID","Eplant")
  

#calculamos media y desviación estandar/mes

  group_by(point_id,month) %>%
  mutate(eplant_monthly_mean=mean(e_plant,na.rm=T))  %>%
  mutate(epalnt_z=(e_plant-eplant_monthly_mean)/sd(e_plant,na.rm=T)) 
 
 random_data <- random_data %>%
    filter(ID=="P00453") %>%
ggplot(random_data,aes(x=day,y=Eplant))+
  geom_point()+
  theme_bw()+
  theme(panel.grid = element_blank())

random_data <- group_by("point_id")
