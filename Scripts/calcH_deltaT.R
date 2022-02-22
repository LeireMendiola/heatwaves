library(tidyverse)
library(bigleaf)
library(lubridate)

##load full dataset

full_data <- read.csv("Data/Cleaned/lst_nfi_Eplant_rew_meteo_lidar.csv")

full_data <- full_data %>%
  select(-c("X","...1.x"))

##create a dataset only with 05/01/2021-08/31/2021 data

data_21 <- filter(full_data, Date >= as.Date("2021-05-01"), Date <= as.Date("2021-09-30"))

#we select only the variables we need

data_21 <- data_21 %>%
  select("ID", "Date", "LST_modis", "dbh", "density", "lai", "topo_altitude_asl","HM", "MaxTemperature", "MinRelativeHumidity", "WindSpeed" )


### calculate sensible heat from DeltaT step by step ###

##calculate VPDmax from Tmax and RHmin
data_21 <- data_21 %>% mutate(VPDmax = plantecophys::RHtoVPD(RH=MinRelativeHumidity, TdegC =MaxTemperature , Pa = 101))   #asumimos que en Tmax RHmin coinciden en el tiempo 

##mirar por que tengo tantos NAs
 
# calculate air pressure from altitude

data_21 <- data_21 %>% 
  mutate(airP = bigleaf::pressure.from.elevation(elev = topo_altitude_asl,
                                                 Tair = MaxTemperature, VPD = VPDmax, constants = bigleaf::bigleaf.constants()))

# calculate air density for a given air temperature (Tair) and pressure

data_21 <- data_21 %>% 
  mutate(rho = bigleaf::air.density(Tair = MaxTemperature, pressure = airP, constants = bigleaf::bigleaf.constants()))

# calculate zero plane displacement, d, from vegetation height as d = 2/3 VH, where VH is mean vegetation height (Brutsaert 1982)

data_21 <- data_21 %>%  mutate(d = (2/3)*HM)

# calculate the momentum roughness parameter in m, z0m, as z0m = 0.1 VH (Brutsaert 1982)

data_21 <- data_21 %>% mutate(z0m = 0.1*HM)

# calculate friction velocity (u*, here uS) from vegetation height and wind speed

#but first Adrià's correction for wind velocity

data_21 <- data_21 %>% mutate(windS_correct = 1.635*WindSpeed - 0.1405*(WindSpeed)^2 + 0.00637*(WindSpeed)^3 + 0.3416)


# Eqn 5.1 in Campbell & Norman 1998

data_21 <- data_21 %>%
  mutate(uS = 0.4 * windS_correct/log((HM + 2 - d)/z0m))

# calculate resistance to heat transfer (rH) from Eqn 5 in Knauer et al. 2017 GCB (rH = 1/Ga) CORRIGENDUM

# rH calculated from wind speed u (m/s) and friction velocity uS (m/s) under the assumption of a logarithmic wind profile above the canopy

calcrH <- function(u, uS){
  rH <- (u/uS^2) + 6.2 * uS^-0.67
  return(rH)
}
data_21 <- data_21 %>% mutate(rH = calcrH(u = windS_correct, uS = uS))

# heat capacity of air in J kg-1 K-1
cp <- 1003.5

# calculate sensible heat flux from the temperature difference between the leaf and the air:

# Eqn 4 in Muller et al 2021 New Phytologist
calcH <- function(cp, rho, dT, rH){
  H <- cp * rho * (dT/rH)
  return(H)
}

#we need to add DeltaT

data_21 <- data_21 %>% mutate (DeltaT = LST_modis - (MaxTemperature+273))

data_21 <- data_21 %>% 
  mutate(H = calcH(cp = cp, rho = rho, dT = DeltaT, rH = rH))

#explore H values 
hist(data_21$H_neg)

H_neg <- data_21[data_21$H < 0, ] #almost 50% of observations are negative values


# get rid of unreasonable values
data_21[which(data_21$H > 1500), 'H'] <- NA  #0.92% of observations are NA

### Convert Eplant from mm day-1 to W m-2 as an aproximation to latent heat flux (LE)

data_21 <- data_21 %>%
  mutate(LE = Eplant * 2.45*10^6/24/3600)

hist(data_21$LE)

#before modeling save 2021 summer database with heat fluxes

write.csv(data_21, "Data/Cleaned/HeatFlux_21.csv")

#min(e_plant$Date)

### sensible heat from energy budget
plantMod <- plantMod %>%
  mutate( H_est = Radiation - LE)

hist(plantMod$H_est)
plot(plantMod$H_est ~ plantMod$H)
