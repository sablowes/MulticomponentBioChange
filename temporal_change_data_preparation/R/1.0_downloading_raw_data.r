# Downloading raw data

if(!dir.exists('temporal_change_data_preparation/data/raw data/'))   dir.create('temporal_change_data_preparation/data/raw data/')
listF <- list.files('temporal_change_data_preparation/R/data download', pattern = ".R|.r", full.names = TRUE)
lapply(listF, function(fullPath) source(fullPath, encoding = 'UTF-8', echo = FALSE, local = TRUE))
