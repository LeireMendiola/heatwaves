
###obrim paquets necessaris
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(broom.mixed)
library(performance)
library(lme4)
library(MuMIn)
library(lmerTest)
library(ggplot2)
library(scales)
library(Matrix)
library(lme4)
library(dplyr)
library(viridisLite)
library(viridis)
library(qqplotr)
library(ggpubr)
library(sjPlot)  
library(factoextra)
library(lfcdata)

hotdays

# PCA ---------------------------------------------------------------------

#hotdays <- read.csv("hotdays_Leire_20211103.csv")

hotdays <- hotdays %>%
  mutate(relative_dt=diffdT/climTanom_abs)

geo_pca <- hotdays %>%
  filter(!DeltaT=="NA")%>%
  dplyr::select("relative_dt","Altitude","Slope","topo_fdm_curvature","eastness","northness")

geo.pca <- prcomp(geo_pca[,-(1)], scale = T)
get_pca_var(geo.pca)$cos2
fviz_eig(geo.pca)

fviz_pca_var(geo.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)

geo_dim2<-as.data.frame(get_pca_ind(geo.pca)$coord[,c(1,2)])
names(geo_dim2)<-c("geo_dim1","geo.dim2")

geo_dim2<-as.data.frame(cbind(geo_dim2,geo_pca$relative_dt))
names(geo_dim2)<-c("geo_dim1","geo_dim2","relative_dt")

summary(m<-lm(relative_dt~geo_dim1+geo_dim2,geo_dim2))

###forest structure

str_pca <- hotdays %>%
  filter(!DeltaT=="NA"&!HM=="NA")%>%
  dplyr::select("relative_dt","HM","BF","AB","REC","LAI","Basal_area","density")

str.pca <- prcomp(str_pca[,-1], scale = T)
get_pca_var(str.pca)$cos2
fviz_eig(str.pca)

fviz_pca_var(str.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

str_dim2<-as.data.frame(get_pca_ind(str.pca)$coord[,c(1,2)])

str_dim2<-as.data.frame(cbind(str_dim2,str_pca$relative_dt))
names(str_dim2)<-c("str_dim1","str_dim2","relative_dt")

summary(m<-lm(relative_dt~str_dim1+str_dim2,data=str_dim2))

###meteo

met_pca<-hotdays%>%
  ungroup() %>%
  dplyr::select("relative_dt","MaxVPD","P_PET7","WindSpeed","clim_prec_year","clim_tmax_sum")%>%
  drop_na()

met.pca <- prcomp(met_pca[,-1], scale = T)
get_pca_var(met.pca)$cos2
fviz_eig(met.pca)

fviz_pca_var(met.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

met_dim2<-as.data.frame(get_pca_ind(met.pca)$coord[,c(1,2)])

met_dim2<-as.data.frame(cbind(met_dim2,met_pca$relative_dt))
names(met_dim2)<-c("met_dim1","met_dim2","relative_dt")

summary(m<-lm(relative_dt~met_dim1+met_dim2,data=met_dim2))

###all together

forest_cat<- hotdays %>%
  ungroup()%>%
  dplyr::select("relative_dt","Altitude","Slope","topo_fdm_curvature","eastness","northness",
                "HM","BF","AB","REC","LAI","Basal_area","density",
                "MaxVPD","P_PET7","WindSpeed","clim_prec_year","clim_tmax_sum")%>%
  drop_na()

all.pca <- prcomp(forest_cat[,-1], scale = T)
get_pca_var(all.pca)$cos2
fviz_eig(all.pca)

fviz_pca_var(all.pca,axes=c(1,2),
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


all_dim2<-as.data.frame(get_pca_ind(all.pca)$coord[,c(1,2)])

all_dim2<-as.data.frame(cbind(all_dim2,forest_cat$relative_dt))
names(all_dim2)<-c("all_dim1","all_dim2","relative_dt")

summary(m<-lm(relative_dt~all_dim1+all_dim2,all_dim2))

all_dim2 <- str_dim2 %>% left_join (geo_dim2,by="relative_dt") %>%
  left_join (met_dim2,by="relative_dt")

summary(lm(relative_dt~str_dim1+str_dim2+geo_dim1+geo_dim2+met_dim1+met_dim2,all_dim2))


