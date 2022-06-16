# Wrangle raw data

if (!dir.exists(paste0(getwd(),"/data/wrangled data")))   dir.create(paste0(getwd(),"/data/wrangled data"))
listF <- list.files(paste0(getwd(), "/R/data wrangling"), pattern = ".R|.r", full.names = TRUE)
for (fullpath in listF) {
  print(fullpath)
  source(file = fullpath, encoding = 'UTF-8', echo = FALSE, local = TRUE)
}
