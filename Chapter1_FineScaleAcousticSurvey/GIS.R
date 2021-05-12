#GIS

library(sf)
library(raster)
library(tidyverse)
library(RColorBrewer)
library(sp)

water <- st_read("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/marina_bowra_gis_files_062019/1_WatercourseLines_3857_BowraClip/WatercourseLines_3857_BowraClip.shp")
plot(water)

points <- st_read("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/marina_bowra_gis_files_062019/3_FinalTransects_3857/FinalTransects_3857.shp")

distance <- as.data.frame(st_distance(points, water))

total_land <- cbind.data.frame(points$Point, distance$V8) %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/26.01.2021_DistWater_Transects.csv", row.names = F)

