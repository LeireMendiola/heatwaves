library(tidyverse)

### clean and read the data ###

plant <- read.csv('data/e_plant_2021.csv')
plant <- plant %>%
  dplyr::mutate(Date = lubridate::ymd(day)) %>% 
  dplyr::rename(ID = point_id) %>% 
  dplyr::select(-c(X, day)) 
plant$ID <- plant$ID %>% str_replace("_", "")

full <- read.csv('data/full_dataset_modelled_lst.csv')
full <- full %>%
  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
  dplyr::select(-c(X.1, X, date))

nfi <- readxl::read_excel('data/20201027_nfi_data.xlsx')
nfi <- nfi %>% 
  dplyr::rename(ID = plot_id) %>%
  select(c(ID, dbh, density, lai, topo_altitude_asl))
nfi$ID <- nfi$ID %>% str_replace('_', '')

lidar <- read.csv('data/lidar_nfi_mod.csv')
lidar <- lidar %>% dplyr::rename(ID = plot_id)
lidar$ID <- lidar$ID %>% str_replace('_', '')

# merge the datasets by ID and date
plantMod <- dplyr::left_join(plant, full, by = c('ID', 'Date'))
plantMod <- dplyr::left_join(plantMod, nfi, by = c('ID'))
plantMod <- dplyr::left_join(plantMod, lidar, by = c('ID'))

### calculate sensible heat from DeltaT step by step ###

# calculate air pressure from altitude
plantMod <- plantMod %>% 
  mutate(airP = bigleaf::pressure.from.elevation(elev = topo_altitude_asl,
                                                 Tair = MaxTemperature, VPD = MaxVPD, constants = bigleaf::bigleaf.constants()))
# calculate air density for a given air temperature (Tair) and pressure
plantMod <- plantMod %>% 
  mutate(rho = bigleaf::air.density(Tair = MaxTemperature, pressure = airP, constants = bigleaf::bigleaf.constants()))
# calculate zero plane displacement, d, from vegetation height as d = 2/3 VH, where VH is mean vegetation height (Brutsaert 1982)
plantMod <- plantMod %>%  mutate(d = (2/3)*HM)
# calculate the momentum roughness parameter in m, z0m, as z0m = 0.1 VH (Brutsaert 1982)
plantMod <- plantMod %>% mutate(z0m = 0.1*HM)
# calculate friction velocity (u*, here uS) from vegetation height and wind speed
# Eqn 5.1 in Campbell & Norman 1998
plantMod <- plantMod %>%
  mutate(uS = 0.4 * WindSpeed/log((HM + 2 - d)/z0m))
# caclculate resistance to heat transfer (rH) from Eqn 5 in Knauer et al. 2017 GCB (rH = 1/Ga) CORRIGENDUM
# rH calculated from wind speed u (m/s) and friction velocity uS (m/s) under the assumption of a logarithmic wind profile above the canopy
calcrH <- function(u, uS){
  rH <- (u/uS^2) + 6.2 * uS^-0.67
  return(rH)
}
plantMod <- plantMod %>% mutate(rH = calcrH(u = WindSpeed, uS = uS))
# heat capacity of air in J kg-1 K-1
cp <- 1003.5
# calculate sensible heat flux from the temperature difference between the leaf and the air:
# Eqn 4 in Muller et al 2021 New Phytologist
calcH <- function(cp, rho, dT, rH){
  H <- cp * rho * (dT/rH)
  return(H)
}
plantMod <- plantMod %>% 
  mutate(H = calcH(cp = cp, rho = rho, dT = DeltaT, rH = rH))
# get rid of unreasonable values
plantMod[which(plantMod$H > 1500), 'H'] <- NA

### Convert Eplant from mm day-1 to W m-2

plantMod <- plantMod %>%
  mutate(LE = Eplant * 2.45*10^6/24/3600)

hist(plantMod$LE)

#min(e_plant$Date)


### sensible heat from energy budget
plantMod <- plantMod %>%
  mutate( H_est = Radiation - LE)

hist(plantMod$H_est)
plot(plantMod$H_est ~ plantMod$H)
