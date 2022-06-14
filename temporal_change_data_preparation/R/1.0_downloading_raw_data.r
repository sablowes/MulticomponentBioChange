# Downloading raw data

if(!dir.exists('temporal_comparison_data_preparation/data/raw data/'))   dir.create('temporal_comparison_data_preparation/data/raw data/')
listF <- list.files('temporal_comparison_data_preparation/R/data download', pattern = ".R|.r", full.names = TRUE)
lapply(listF, function(fullPath) source(fullPath, encoding = 'UTF-8', echo = FALSE, local = TRUE))
