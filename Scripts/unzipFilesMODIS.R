library(tidyverse)
# create a vector with all the zip files
# enter your the name of the folder where you have the data accordingly
zipFileNames <- paste0("Data/inputs/AppEEARS/AquaTerraZipped//" , list.files('Data/inputs/AppEEARS/AquaTerraZipped/'))

# create a vector (from a list) with only the names of the *.csv (what we want)
csvFileNames <- unlist(map(zipFileNames, ~ unzip(zipfile = .x, list = T)[1, 1]))

# extract the files onto a newly created folder within the data folder
purrr::map2(zipFileNames, csvFileNames, ~unzip(zipfile = .x, file = .y, exdir = 'Data/inputs/AppEEARS/AquaTerraZipped/AquaTerraUnzipped2'))
# map2 allows to pass two arguments

csvFileNames2 <- list.files (path = "Data/inputs/AppEEARS/AquaTerraZipped/AquaTerraUnzipped",pattern = "aqua" )
