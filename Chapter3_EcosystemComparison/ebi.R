library(raster)
library(rgdal)
library(tidyverse)

b1_files <- list.files('D:/planet_data/SERF', pattern = glob2rx("*B1_(Raw).tiff"), full.names = T, recursive = T)
b2_files <- list.files('D:/planet_data/SERF', pattern = glob2rx("*B2_(Raw).tiff"), full.names = T, recursive = T)
b3_files <- list.files('D:/planet_data/SERF', pattern = glob2rx("*B3_(Raw).tiff"), full.names = T, recursive = T)
#b4_files <- list.files('D:/planet_data/SERF', pattern = glob2rx("*B4_(Raw).tiff"), full.names = T, recursive = T)

b_all <- c(b1_files, b2_files, b3_files, b4_files)
#b_all <- c(b3_files, b4_files)

for (b in 1:length(b_all)) {
  
  
  b1 <- raster(b_all[b])
    
    b2 <- raster(b_all[b+73])
      
        b3 <- raster(b_all[b+146])
  
  ebi <- (b3+b2+b1)/((b2/b1)*(b3-b1+1)) 
        
        file_base <- strsplit(b_all[b], split = "/")
        string1 <- paste(file_base[[1]][1], file_base[[1]][2], file_base[[1]][3], file_base[[1]][4], sep = "/")
        string2 <- paste("/", file_base[[1]][4], "_ebi.tiff", sep = "")
        file_name <- paste(string1, string2, sep = "")
        
        
        #writeRaster(ebi, file_name)
  
}

#b1_files <- list.files('D:/planet_data/SERF', pattern = glob2rx("*B1_(Raw).tiff"), full.names = T, recursive = T)
#b2_files <- list.files('D:/planet_data/SERF', pattern = glob2rx("*B2_(Raw).tiff"), full.names = T, recursive = T)
b3_files <- list.files('D:/planet_data/Eungella', pattern = glob2rx("*B3_(Raw).tiff"), full.names = T, recursive = T)
b4_files <- list.files('D:/planet_data/Eungella', pattern = glob2rx("*B4_(Raw).tiff"), full.names = T, recursive = T)

#b_all <- c(b1_files, b2_files, b3_files, b4_files)
b_all <- c(b3_files, b4_files)

for (b in 1:length(b_all)) {
  
  
  b3 <- raster(b_all[b])
  
  b4 <- raster(b_all[b+66])
  
  
  msavi <- (((2*b4)+1) - sqrt((((2*b4+1)^2)-8*(b4-b3)))/2)
  
  file_base <- strsplit(b_all[b], split = "/")
  string1 <- paste(file_base[[1]][1], file_base[[1]][2], file_base[[1]][3], file_base[[1]][4], sep = "/")
  string2 <- paste("/", file_base[[1]][4], "_msavi.tiff", sep = "")
  file_name <- paste(string1, string2, sep = "")
  
  
  writeRaster(msavi, file_name)
  
}

rm(list=ls())

ebi_list <- list.files('D:/planet_data', pattern = glob2rx("*msavi.tif"), full.names = T, recursive = T)

df <- data.frame(FileName = character(),
                 mean_ebi = integer(),
                 max_ebi = integer(),
                 min_ebi = integer())


for (raster in ebi_list) {
  read_in <- raster(raster)
  
  values <- getValues(read_in)
  
  msavi_mean <- mean(values, na.rm = T)
  msavi_max <- max(values, na.rm = T)
  msavi_min <- min(values, na.rm = T)
  
  df_temp <- data.frame(read_in@file@name, msavi_mean, msavi_max, msavi_min)
  
  df <- rbind(df_temp, df)
  
  
}

#write.csv(df, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/07.04.2022_msavi.csv")


rm(list=ls())

ebi_list <- list.files('D:/planet_data', pattern = glob2rx("*ebi.tif"), full.names = T, recursive = T)

df <- data.frame(FileName = character(),
                 mean_ebi = integer(),
                 max_ebi = integer(),
                 min_ebi = integer())


for (raster in ebi_list) {
  read_in <- raster(raster)
  
  values <- getValues(read_in)
  
  ebi_mean <- mean(values, na.rm = T)
  ebi_max <- max(values, na.rm = T)
  ebi_min <- min(values, na.rm = T)
  
  df_temp <- data.frame(read_in@file@name, ebi_mean, ebi_max, ebi_min)
  
  df <- rbind(df_temp, df)
  
  
}

#write.csv(df, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_ebi.csv")

#NDVI ----
rm(list=ls())

ndvi_list <- list.files('D:/planet_data', pattern = glob2rx("*_NDVI.tiff"), full.names = T, recursive = T)

df <- data.frame(FileName = character(),
                 mean_ndvi = integer(),
                 max_ndvi = integer(),
                 min_ndvi = integer())


for (raster in ndvi_list) {
  read_in <- stack(raster)
  
  values <- getValues(read_in@layers[[2]])
  
  ndvi_mean <- mean(values, na.rm = T)
  ndvi_max <- max(values, na.rm = T)
  ndvi_min <- min(values, na.rm = T)
  
  df_temp <- data.frame(raster, ndvi_mean, ndvi_max, ndvi_min)
  
  df <- rbind(df_temp, df)
  
  
}

# write.csv(df, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/06.05.2022_ndvi.csv")


#NDWI ----
rm(list=ls())

ndwi_list <- list.files('D:/planet_data', pattern = glob2rx("*NDWI.tiff"), full.names = T, recursive = T)

df <- data.frame(FileName = character(),
                 mean_ndwi = integer(),
                 max_ndwi = integer(),
                 min_ndwi = integer())


for (raster in ndwi_list) {
  read_in <- stack(raster)
  
  values <- getValues(read_in@layers[[2]])
  
  ndwi_mean <- mean(values, na.rm = T)
  ndwi_max <- max(values, na.rm = T)
  ndwi_min <- min(values, na.rm = T)
  
  df_temp <- data.frame(raster, ndwi_mean, ndwi_max, ndwi_min)
  
  df <- rbind(df_temp, df)
  
  
}

#write.csv(df, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_ndwi.csv")

rm(list=ls())

df_ndwi <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_ndwi.csv")

df_ndwi <- separate(df_ndwi, raster, into = c("drive", "folder", "site", "point_date", "filename"), sep = "/", remove = F) %>% 
  select(raster, site, point_date, ndwi_mean, ndwi_max, ndwi_min) %>% 
  separate(point_date, into = c("point", "date"), sep = "_", remove = T) %>% 
  rename("filename" = raster)

#future and past ndvi; for redo original df stop at line 193
df_ndvi <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/06.05.2022_ndvi.csv")

df_ndvi <- separate(df_ndvi, raster, into = c("drive", "folder", "site", "point_date", "filename"), sep = "/", remove = F) %>% 
  dplyr::select(raster, site, point_date, ndvi_mean, ndvi_max, ndvi_min) %>% 
  separate(point_date, into = c("point", "date"), sep = "_", remove = T) %>% 
  rename("filename" = raster) %>% 
  separate(date, into = c("year", "month_date"), sep = 4, remove = F) %>% 
  separate(month_date, into = c("month", "day"), sep = 2) %>% 
  group_by(site, point, year, month) %>% 
  summarise(mean = mean(ndvi_mean)) %>% 
  filter(year == "2020") %>% 
  filter(month == "07" | month == "08" | month == "09" | month == "10" | month == "11" | month == "12") %>% 
  mutate(site = case_when(site == "chp3" ~ "SERF",
                          TRUE ~ site)) %>% 
  pivot_wider(., names_from = "month", values_from = "mean") %>% 
  dplyr::select(., site, point, `07`, `11`) %>% 
  rename("past_ndvi" = `07`,
         "future_ndvi" = `11`)

# write.csv(df_ndvi, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/06.05.2022_pastfuture_ndvi.csv")
  
df_ebi <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_ebi.csv")

df_ebi <- separate(df_ebi, read_in.file.name, into = c("drive", "folder", "site", "point_date", "filename"), sep = "\\\\", remove = F) %>% 
  select(read_in.file.name, site, point_date, ebi_mean, ebi_max, ebi_min) %>% 
  separate(point_date, into = c("point", "date"), sep = "_", remove = T) %>% 
  rename("filename" = read_in.file.name)

df_msavi <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/07.04.2022_msavi.csv")

df_msavi <- separate(df_msavi, read_in.file.name, into = c("drive", "folder", "site", "point_date", "filename"), sep = "\\\\", remove = F) %>% 
  select(read_in.file.name, site, point_date, msavi_mean, msavi_max, msavi_min) %>% 
  separate(point_date, into = c("point", "date"), sep = "_", remove = T) %>% 
  rename("filename" = read_in.file.name)

joined_df <- left_join(df_ebi,df_ndvi, by = c("site", "point", "date"))
joined_df <- left_join(joined_df, df_ndwi, by = c("site", "point", "date"))
joined_df <- left_join(joined_df, df_msavi, by = c("site", "point", "date"))

joined_df <- select(joined_df, site, point, date, ebi_mean, ebi_max, ebi_min, ndvi_mean, ndvi_max, ndvi_min, ndwi_mean, ndwi_max, ndwi_min, msavi_mean, msavi_max, msavi_min) %>% 
  na.exclude() %>% 
  filter(point != "EO") %>% 
  filter(!str_detect(date, "^202012")) %>% 
  filter(!str_detect(date, "^202017")) %>% 
  filter(!str_detect(date, "^2021"))

summary(joined_df)

cor.test(joined_df$ebi_mean, joined_df$ebi_max)
cor.test(joined_df$ebi_mean, joined_df$ebi_min)
cor.test(joined_df$ebi_mean, joined_df$ndvi_mean)
cor.test(joined_df$ebi_mean, joined_df$ndvi_max)
cor.test(joined_df$ebi_mean, joined_df$ndvi_min)
cor.test(joined_df$ebi_mean, joined_df$ndwi_mean)
cor.test(joined_df$ebi_mean, joined_df$ndwi_max)
cor.test(joined_df$ebi_mean, joined_df$ndwi_min)
cor.test(joined_df$ebi_mean, joined_df$msavi_mean)
cor.test(joined_df$ebi_mean, joined_df$msavi_max)
cor.test(joined_df$ebi_mean, joined_df$msavi_min)

cor.test(joined_df$ebi_max, joined_df$ebi_min)
cor.test(joined_df$ebi_max, joined_df$ndvi_mean)
cor.test(joined_df$ebi_max, joined_df$ndvi_max)
cor.test(joined_df$ebi_max, joined_df$ndvi_min)
cor.test(joined_df$ebi_max, joined_df$ndwi_mean)
cor.test(joined_df$ebi_max, joined_df$ndwi_max)
cor.test(joined_df$ebi_max, joined_df$ndwi_min)
cor.test(joined_df$ebi_max, joined_df$msavi_mean)
cor.test(joined_df$ebi_max, joined_df$msavi_max)
cor.test(joined_df$ebi_max, joined_df$msavi_min)

cor.test(joined_df$ebi_min, joined_df$ndvi_mean)
cor.test(joined_df$ebi_min, joined_df$ndvi_max)
cor.test(joined_df$ebi_min, joined_df$ndvi_min)
cor.test(joined_df$ebi_min, joined_df$ndwi_mean)
cor.test(joined_df$ebi_min, joined_df$ndwi_max)
cor.test(joined_df$ebi_min, joined_df$ndwi_min)
cor.test(joined_df$ebi_min, joined_df$msavi_mean)
cor.test(joined_df$ebi_min, joined_df$msavi_max)
cor.test(joined_df$ebi_min, joined_df$msavi_min)

cor.test(joined_df$ndvi_mean, joined_df$ndvi_max)
cor.test(joined_df$ndvi_mean, joined_df$ndvi_min)
cor.test(joined_df$ndvi_mean, joined_df$ndwi_mean)
cor.test(joined_df$ndvi_mean, joined_df$ndwi_max)
cor.test(joined_df$ndvi_mean, joined_df$ndwi_min)
cor.test(joined_df$ndvi_mean, joined_df$msavi_mean)
cor.test(joined_df$ndvi_mean, joined_df$msavi_max)
cor.test(joined_df$ndvi_mean, joined_df$msavi_min)


cor.test(joined_df$ndvi_max, joined_df$ndvi_min)
cor.test(joined_df$ndvi_max, joined_df$ndwi_mean)
cor.test(joined_df$ndvi_max, joined_df$ndwi_max)
cor.test(joined_df$ndvi_max, joined_df$ndwi_min)
cor.test(joined_df$ndvi_max, joined_df$msavi_mean)
cor.test(joined_df$ndvi_max, joined_df$msavi_max)
cor.test(joined_df$ndvi_max, joined_df$msavi_min)


cor.test(joined_df$ndvi_min, joined_df$ndwi_mean)
cor.test(joined_df$ndvi_min, joined_df$ndwi_max)
cor.test(joined_df$ndvi_min, joined_df$ndwi_min)
cor.test(joined_df$ndvi_min, joined_df$msavi_mean)
cor.test(joined_df$ndvi_min, joined_df$msavi_max)
cor.test(joined_df$ndvi_min, joined_df$msavi_min)


cor.test(joined_df$ndwi_mean, joined_df$ndwi_max)
cor.test(joined_df$ndwi_mean, joined_df$ndwi_min)
cor.test(joined_df$ndwi_mean, joined_df$msavi_mean)
cor.test(joined_df$ndwi_mean, joined_df$msavi_max)
cor.test(joined_df$ndwi_mean, joined_df$msavi_min)

cor.test(joined_df$ndwi_max, joined_df$ndwi_min)
cor.test(joined_df$ndwi_max, joined_df$msavi_mean)
cor.test(joined_df$ndwi_max, joined_df$msavi_max)
cor.test(joined_df$ndwi_max, joined_df$msavi_min)


cor.test(joined_df$ndwi_min, joined_df$msavi_mean)
cor.test(joined_df$ndwi_min, joined_df$msavi_max)
cor.test(joined_df$ndwi_min, joined_df$msavi_min)

cor.test(joined_df$msavi_mean, joined_df$msavi_max)
cor.test(joined_df$msavi_mean, joined_df$msavi_min)

cor.test(joined_df$msavi_max, joined_df$msavi_min)



joined_df %>% select(site, point, date, ndvi_mean, ebi_max, ndwi_mean) %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/07.04.2022_satelliteindices_final.csv")

