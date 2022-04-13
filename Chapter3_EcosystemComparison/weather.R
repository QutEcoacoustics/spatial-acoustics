rm(list = ls())

library(tidyverse)

#Booroopki ----
booroopki <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_weatherBooroopki.csv") %>% 
  select(Date, Day, TempMin, TempMax, Rain, Lat, Long, Site)

eungella <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_weatherEungella.csv") %>% 
  select(Date, Day, TempMin, TempMax, Rain, Lat, Long, Site)

serf <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_weatherSERF.csv") %>% 
  select(Date, Day, TempMin, TempMax, Rain, Lat, Long, Site)

bowra <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_weatherBowra.csv") %>% 
  select(Date, Day, TempMin, TempMax, Rain, Lat, Long, Site)

bonbon <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_weatherBonBon.csv") %>% 
  select(Date, Day, TempMin, TempMax, Rain, Lat, Long, Site)

df_total <- rbind(booroopki, eungella, serf, bowra, bonbon)

#write.csv(df_total, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_weathertotal.csv")

cor.test(df_total$TempMin, df_total$Lat)
cor.test(df_total$TempMin, df_total$Long)

cor.test(df_total$TempMax, df_total$Lat)
cor.test(df_total$TempMax, df_total$Long)

cor.test(df_total$Rain, df_total$Lat)
cor.test(df_total$Rain, df_total$Long)

weather <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_weathertotal.csv")

weather$date  <- as.factor(weather$date)

points <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/Points.csv") %>% 
  select(Site.name, Point, Site.., lat, long, Ecoregion, CanopyHeight, CanopyCover, Grazing, Logging, Fire.age, Storm.damage.age)

satellite <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/07.04.2022_satelliteindices_final.csv") %>% 
  select(date, site, point, ndvi_mean, ebi_max, ndwi_mean)

joined <- left_join(points, satellite, by = c("Site.name" = "site", "Point" = "point"))
 
joined$date  <- as.factor(joined$date)

df_merged <- select(weather, date, Rain, Lat, Long, Site, TempMax, TempMin) %>% 
  left_join(joined, by = c("date", "Site" = "Site.name"))

cor.test(df_merged$ndvi_mean, df_merged$Lat)
cor.test(df_merged$ndvi_mean, df_merged$Long)
cor.test(df_merged$ndvi_mean, df_merged$Rain)
cor.test(df_merged$ndwi_mean, df_merged$Lat)
cor.test(df_merged$ndwi_mean, df_merged$Long)
cor.test(df_merged$ndvi_mean, df_merged$Rain)
cor.test(df_merged$ebi_max, df_merged$Lat)
cor.test(df_merged$ebi_max, df_merged$Long)
cor.test(df_merged$ebi_max, df_merged$Rain)

cor.test(df_merged$Rain, df_merged$Lat)
cor.test(df_merged$Rain, df_merged$Long)
cor.test(df_merged$TempMax, df_merged$Lat)
cor.test(df_merged$TempMax, df_merged$Long)
cor.test(df_merged$TempMin, df_merged$Lat)
cor.test(df_merged$TempMin, df_merged$Long)


cor.test(df_merged$TempMax, df_merged$ndvi_mean)
cor.test(df_merged$TempMax, df_merged$ndwi_mean)
cor.test(df_merged$TempMax, df_merged$ebi_max)
cor.test(df_merged$TempMin, df_merged$ndvi_mean)
cor.test(df_merged$TempMin, df_merged$ndwi_mean)
cor.test(df_merged$TempMin, df_merged$ebi_max)
cor.test(df_merged$Rain, df_merged$ndvi_mean)
cor.test(df_merged$Rain, df_merged$ndwi_mean)
cor.test(df_merged$Rain, df_merged$ebi_max)

df_merged %>% select(everything(), -c(TempMax, TempMin)) %>% 
write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/08.04.2022_satelliteindices_final.csv")

library(ggplot2)

df_merged %>% 
  ggplot(aes(x = date))+
  geom_jitter(aes(y = TempMax), colour = "orange") +
  geom_jitter(aes(y = TempMin), colour = "blue") +
  facet_wrap(.~Site)

satellite2 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/05.04.2022_satelliteindices_final.csv") %>% 
  select(date, site, ndvi_mean, Lat_point, Long_point, point)

satellite2 %>% filter(date >= 20200701 & date <= 20201101) %>% 
  ggplot(aes(x = date))+
  geom_jitter(aes(y = ndvi_mean), colour = "green") +
  facet_grid(site~point)




