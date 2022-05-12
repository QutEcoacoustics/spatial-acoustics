library(tidyverse)
library(ggplot2)

rm(list = ls())

set.seed(123)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

df_complete <- read.csv(getDataPath("15.04.2022_completedf.csv"))

#Phenology df ----
weather <- read.csv(getDataPath("12.04.2022_weather.csv")) %>% 
  select(everything(), -X) %>% 
  separate(date, into = c("year", "month_day"), sep = 4, remove = F) %>% 
  separate(month_day, into = c("month", "day"), sep = 2) %>% 
  mutate(date_r = paste(year, month, day, sep = "-"))

weather$point <- tolower(weather$point)
weather$date_r <- as.Date.character(weather$date_r)



summary(df_complete)

df_complete$date_r <- as.Date.character(df_complete$date_r)

df_complete <- full_join(weather, df_complete, by = c("site" = "Site.name", "point" = "Point", "date_r"))


df_complete$point <- as.factor(df_complete$point)

satellite <- read.csv(getDataPath("07.04.2022_satelliteindices_final.csv"))%>% 
  select(everything(), -X) %>% 
  separate(date, into = c("year", "month_day"), sep = 4, remove = F) %>% 
  separate(month_day, into = c("month", "day"), sep = 2) %>% 
  mutate(date_r = paste(year, month, day, sep = "-"))

satellite$date_r <- as.Date.character(satellite$date_r)
satellite$site <- as.factor(satellite$site)
df_complete$site <- as.factor(df_complete$site)

df_complete <- full_join(satellite, df_complete, by = c("site", "point", "date_r")) %>% 
  filter(RFclass != "NA")

df_complete <- select(df_complete, site, point, date_r, ID.x, date_time2, lat, lon, RFclass, general_category, ndvi_mean, ebi_max, ndwi_mean, temp_max, temp_min, rain_value, moon_illu, period, week_day, -c(X, date, year, month, day, year.x, month.x, day.x), everything())

summary(df_complete)

df_complete <- select(df_complete, everything(), -c(date.x, year.x, month.x, day.x, moon_illumination, X, date.y, year.y, month.y, day.y, ID.y))

summary(df_complete)

#write.csv(df_complete, getDataPath("19.04.2022_dfphenolgy.csv"), row.names = F)

df_complete <- read.csv(getDataPath("15.04.2022_completedf.csv"))


largerbuffer_files <- list.files('D:/Chapter3_EcosystemComparison/6_LandscapeMetrics', pattern = glob2rx("*3k.csv"), full.names = T, recursive = T)

read_in <- lapply(largerbuffer_files, read.csv)

final_df <- rbind(read_in[[1]], read_in[[2]], read_in[[3]], read_in[[4]], read_in[[5]], read_in[[6]], read_in[[7]], read_in[[8]], read_in[[9]], read_in[[10]])

smallerbuffer_files <- list.files('D:/Chapter3_EcosystemComparison/6_LandscapeMetrics', pattern = glob2rx("*325.csv"), full.names = T, recursive = T)

read_in <- lapply(smallerbuffer_files, read.csv)

final_df2 <- rbind(read_in[[1]], read_in[[2]], read_in[[3]], read_in[[4]], read_in[[5]], read_in[[6]], read_in[[7]], read_in[[8]], read_in[[9]], read_in[[10]])

df <- rbind(final_df, final_df2)


df_land <- filter(df, metric == "np" | metric == "contag" | metric == "ca" | metric == "pr" | metric == "tca" | metric == "area" | metric == "ncore") %>% 
  mutate(size = case_when(buffer_size == "dry3k" ~ "3k",
                          buffer_size == "wet3k" ~ "3k",
                          TRUE ~ "325")) %>% 
  mutate(point = case_when(buffer_size == "dry3k" ~ "drya",
                           buffer_size == "wet3k" ~ "weta",
                           buffer_size == "dry325" ~ "drya",
                           buffer_size == "wet325" ~ "weta")) %>% 
  filter(level != "patch")

df_land2 <- df_land %>% mutate(id_landmetrics = paste(.$level, .$class, sep = "_")) %>% 
  mutate(id_landmetrics = case_when(id_landmetrics == "landscape_NA" ~ "landscape",
                                    TRUE ~ id_landmetrics)) %>% 
  mutate(id_landmetrics = paste(.$metric, .$id_landmetrics, sep = "_")) %>% 
  mutate(id_landmetrics = paste(.$id_landmetrics, .$size, sep = "_")) %>% 
  select(value, site, point, id_landmetrics) %>% 
  pivot_wider(., names_from = id_landmetrics, values_from = value)

df_complete$point <- as.factor(df_complete$point)
df_land2$point <- as.factor(df_land2$point)

df_full <- full_join(df_land2, df_complete, by = c("site" = "Site.name", "point" = "Point"))
  #distinct()

bvg <- list.files("D:/Chapter3_EcosystemComparison/BVG/table", pattern = ".csv", full.names = T)

df <- NULL

for (file in bvg) {
  table <- read.csv(file) %>% 
    mutate(info = basename(file))
  
  df <- rbind(table, df)
  
}

bvg_df <- df %>% separate(info, into = c("site", "point", "buffer_size"), sep = "_") %>% 
  separate(buffer_size, into = c("buffer", "ext"), sep = "[.]") %>% 
  select(everything(), -c("count", "ext")) %>% 
  mutate(class_bvg = paste(value, buffer, sep = "_")) %>% 
  select(class_bvg, `m²`, point, site) %>% 
  pivot_wider(., names_from = class_bvg, values_from = `m²`)

df_full2 <- full_join(bvg_df, df_full, by = c("site", "point")) %>% 
  distinct() %>% 
  filter(RFclass != "NA")

test <- full_join(bvg_df, df_land2, by = c("site", "point"))
test <- full_join(test, df_complete, by = c("site", "point")) %>% 
  filter(RFclass != "NA")

#write.csv(test, getDataPath("19.04.2022_df_final.csv"), row.names = F)

#write.csv(df_full2, getDataPath("19.04.2022_dflandscape.csv"))

summary(df_full2)

#write.csv(bvg_df, getDataPath("15.04.2022_bvg_df.csv"), row.names = F)

#bgv_df <- read.csv(getDataPath("15.04.2022_bvg_df.csv"))
  




