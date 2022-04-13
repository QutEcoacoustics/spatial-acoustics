rm(list = ls())

library(tidyverse)
library(ggplot2)
library(raster)
library(sf)
library(rgdal)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}


max_temp <- list.files('D:/Chapter3_EcosystemComparison/weather/max_temp_grid/', pattern = glob2rx("*.grid"), full.names = T, recursive = T)

min_temp <- list.files('D:/Chapter3_EcosystemComparison/weather/min_temp_grid/', pattern = glob2rx("*.grid"), full.names = T, recursive = T)

rain <- list.files('D:/Chapter3_EcosystemComparison/weather/rainfallgrid/', pattern = glob2rx("2020*"), full.names = T, recursive = T)

buffer325 <- st_read("D:/Chapter3_EcosystemComparison/0_Points3857/0_Points3857.shp")

# proj4string(buffer325)
# 
# plot(buffer325)
# 

# 
# ras <- raster(min_temp[1], crs = "EPSG:4326")
# 
# raster::extract(ras, buffer325, method = "simple")
# 
# plot <- plot(ras)
# 
# plot(buffer325$geometry)

df_tempmax <- data.frame(file = character(),
                 temp_max = integer(),
                 ID = character())

for (file in max_temp) {
  
  read_in <- raster(file, crs = "EPSG:4326")
  
  temp_max <- raster::extract(read_in, buffer325, method = "simple")

  value <- data.frame(basename(read_in@file@name), temp_max, buffer325$ID)
  
  df_tempmax <- rbind(df_tempmax, value)
  
}

df_tempmin <- data.frame(file = character(),
                 temp_min = integer(),
                 ID = character())

for (file in min_temp) {
  
  read_in <- raster(file, crs = "EPSG:4326")
  
  temp_min <- raster::extract(read_in, buffer325, method = "simple")
  
  value <- data.frame(basename(read_in@file@name), temp_min, buffer325$ID)
  
  df_tempmin <- rbind(df_tempmin, value)
  
}

df_rain <- data.frame(file = character(),
                         rain = integer(),
                         ID = character())

for (file in rain) {
  
  read_in <- raster(file, crs = "EPSG:4326")
  
  rain_value <- raster::extract(read_in, buffer325, method = "simple")
  
  value <- data.frame(basename(read_in@file@name), rain_value, buffer325$ID)
  
  df_rain <- rbind(df_rain, value)
  
}

df_temp <- left_join(df_tempmax, df_tempmin, by = c("basename.read_in.file.name.", "buffer325.ID")) %>% 
  separate("basename.read_in.file.name.", into = c("basename.read_in.file.name."), sep = 16, remove = T)

df_weather <- left_join(df_temp, df_rain, by = c("basename.read_in.file.name.", "buffer325.ID"))

final <- df_weather %>% rename("file" = `basename.read_in.file.name.`,
              "ID" = buffer325.ID) %>% 
  separate(file, into = c("date", "date1"), sep = 8, remove = T) %>% 
  separate(ID, into = c("site", "point"), sep = "_", remove = F) %>% 
  select(date, site, point, ID, temp_max, temp_min, rain_value)

write.csv(final, getDataPath("12.04.2022_weather.csv"))

ggplot(final, aes(x = date)) +
  geom_point(aes(y = temp_max, colour = "Temperature max")) +
  geom_point(aes(y = temp_min, colour = "Temperature min")) +
  geom_point(aes(y = rain_value, colour = "Rain")) +
  facet_wrap(.~site)

ggsave(getDataPath("Figures", "Exploratory", "12.04.2022_weather_sites.jpg"))


