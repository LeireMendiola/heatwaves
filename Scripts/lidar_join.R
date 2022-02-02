###lidar join

library(readr)
library(tidyverse)

#basal area
basal_area <- read.csv("Data/Lidar/basal_area_lidar.csv")

basal_area <- basal_area %>% select(-"geometry", -"plot_id")
basal_area <- basal_area %>% select(-"AB")
basal_area$ID <- basal_area$ID %>% str_replace("_", "")

colnames(basal_area) <- c("ID", "AB")

#bat
bat <- read.csv("Data/Lidar/bat_lidar.csv")

bat <- bat %>% select(-c("BAT","plot_id", "geometry"))
colnames(bat) <- c("ID", "BAT")     
bat$ID <- bat$ID %>% str_replace("_", "")

ba_bat <- left_join(basal_area,bat, by="ID")

#bf
bf <- read.csv("Data/Lidar/bf_lidar.csv")

bf <- bf %>% select(-c("BF","plot_id", "geometry"))
colnames(bf) <- c("ID", "BF")     
bf$ID <- bf$ID %>% str_replace("_", "")

ba_bat_bf <- left_join(ba_bat,bf, by="ID")

#cat
cat <- read.csv("Data/lidar/cat_lidar.csv")

cat <- cat %>% select(-c("CAT","plot_id", "geometry"))
colnames(cat) <- c("ID", "CAT")     
cat$ID <- cat$ID %>% str_replace("_", "")

ba_bat_bf_cat <- left_join(ba_bat_bf,cat, by="ID")

cat <- read.csv("Data/lidar/cat_lidar.csv")

#dbh
dbh <- read.csv("Data/lidar/dbh_lidar.csv")

dbh <- dbh %>% select(-c("DBH","plot_id", "geometry"))
colnames(dbh) <- c("ID", "DBH")     
dbh$ID <- dbh$ID %>% str_replace("_", "")

ba_bat_bf_cat_dbh <- left_join(ba_bat_bf_cat,dbh, by="ID")

#rec
rec <- read.csv("Data/lidar/rec_lidar.csv")

rec <- rec %>% select(-c("geometry","plot_id","REC"))
colnames(rec) <- c("ID", "REC")     
rec$ID <- rec$ID %>% str_replace("_", "")
ba_bat_bf_cat_dbh_rec <- left_join(ba_bat_bf_cat_dbh,rec, by="ID")



#hm
hm <- read.csv("Data/lidar/hm_plots.csv") #problems: I can´t read ID as variable

hm <- hm %>% select(-"geometry")
colnames(hm) <- c("ID", "HM")     
hm$ID <- hm$ID %>% str_replace("_", "")

ba_bat_bf_cat_dbh_hm <- left_join(ba_bat_bf_cat_dbh,hm, by="ID")


