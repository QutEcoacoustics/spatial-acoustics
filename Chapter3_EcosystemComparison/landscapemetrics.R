#Landscape metrics

rm(list = ls())

library(landscapemetrics)
library(tidyverse)
library(raster)

chapter <- "Chapter3_EcosystemComparison"

getDataPath <- function (...) {
  return(file.path("D:/Chapter3_EcosystemComparison",  ...))
}

files <- list.files(getDataPath(), pattern = glob2rx("*raster_*325.tif"), recursive = T, full.names = T)

file <- files[2]

for (file in files) {
  raster <- raster(file) 
  plot(raster)
  check_landscape(raster, verbose = T)
  resol <- res(raster)
  new_res <- min(resol)

  
  r2 = raster(extent(raster), resolution = new_res, crs = crs(raster))
  r3 = resample(raster, r2, method = "ngb")
  
  lsm_l_tca(raster)
  
  plot(r3)
  check_landscape(r3, verbose = T)
  
  base_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filename(raster)))
  lsm_result <- calculate_lsm(r3)
  lsm_result$raster <- base_name
  final <- separate(lsm_result, raster, into = c("raster", "site", "buffer_size"), sep = "_", remove = F)
  write.csv(x = final, row.names = F, file = getDataPath("6_LandscapeMetrics", paste(base_name, ".csv", sep = "")))
  
}


raster <- raster(getDataPath("5_FiveRivers", "raster_FiveRivers_wet50.tif")) 
plot(raster)
check_landscape(raster, verbose = T)
resol <- res(raster)
new_res <- min(resol)


r2 = raster(extent(raster), resolution = new_res, crs = crs(raster))
r3 = resample(raster, r2, method = "ngb")


plot(r3)
check_landscape(r3, verbose = T)

base_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filename(raster)))
lsm_result <- calculate_lsm(r3)
lsm_result$raster <- base_name
final <- separate(lsm_result, raster, into = c("raster", "site", "buffer_size"), sep = "_", remove = F)
write.csv(x = final, row.names = F, file = getDataPath("6_LandscapeMetrics", paste(base_name, ".csv", sep = "")))
