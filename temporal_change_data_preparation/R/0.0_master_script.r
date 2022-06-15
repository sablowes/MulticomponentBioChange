# 0.0_master_script.r

renv::run(script = "./temporal_change_data_preparation/R/1.0_downloading_raw_data.r", project = paste0(getwd(),"/temporal_change_data_preparation/"))
renv::run(script = "./temporal_change_data_preparation/R/2.0_wrangling_raw_data.r", project = paste0(getwd(),"/temporal_change_data_preparation/"))
renv::run(script = "./temporal_change_data_preparation/R/3.0_merging_long-format_tables.r", project = paste0(getwd(),"/temporal_change_data_preparation/"))

file.copy(from = "./temporal_change_data_preparation/data/long_table.csv", to = "./data/long_table.csv", overwrite = TRUE, copy.date = TRUE)
