library(tidyverse)
# create a vector with all the zip files
# enter your the name of the fodler where you have the data accordingly
zipFileNames <- paste0("211013/" , list.files('211013/'))

# create a vector with only the names of the *.csv (what we want)
csvFileNamesL <- map(zipFileNames, ~ unzip(zipfile = .x, list = T)[1, 1])
csvFileNames <- c()
for (i in 1:length(csvFileNamesL)){
  csvFileNames[i] <- csvFileNamesL[[i]][1]
}

# extract the files onto a newly created folder within the data folder
purrr::map2(zipFileNames, csvFileNames, ~unzip(zipfile = .x, file = .y, exdir = '211013/AquaUnzipped'))
