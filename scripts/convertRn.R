# latent heat of vaporization in J kg-1
cp <- 2.45 * 1e+06
# function to convert transpiration (E) in mm day-1 into W m-2
# mm day-1 is equivalent to L m-2 day-1 or kg m-2 day-1
convertDailyE <- function(E, cp){
  Rn <- E * cp * (1/24) * (1/3600)
  return(Rn)
}