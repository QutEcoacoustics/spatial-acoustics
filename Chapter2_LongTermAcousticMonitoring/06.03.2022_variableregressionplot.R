library(tidyverse)
library(ggplot2)
#Moon investigation

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("24.02.2022_completedf.csv"))

df$RFclass <- as.character(df$RFclass)

df <- mutate(df, class2 = case_when(RFclass == "anthrobird" ~ "bird",
                          RFclass == "anthrobirdfroggeoinsect" ~ "birdfroginsect",
                          RFclass == "anthrobirdgeo" ~ "bird",
                          RFclass == "anthrobirdgeoinsect" ~ "birdinsect",
                          RFclass == "anthrobirdinsect" ~ "birdinsect",
                          RFclass == "anthrofroggeoinsect" ~ "froginsect",
                          RFclass == "anthrogeoinsect" ~ "insect",
                          RFclass == "anthroinsect" ~ "insect",
                          RFclass == "birdgeo" ~ "bird",
                          RFclass == "birdgeoinsect" ~ "birdinsect",
                            TRUE ~ RFclass))

#Hourly Insects ----
insects <- filter(df, RFclass == "insect") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(insects, aes(x = HumOut, y = n)) + 
  geom_smooth( ) +
  ggsave(getDataPath("Figures", "hourlyinsects_humidity.jpg"))


ggplot(insects, aes(x = moon_illu, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "hourlyinsects_moon.jpg"))

#Hourly bird/Insects----
birdinsects <- filter(df, RFclass == "birdinsect") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(birdinsects, aes(x = HumOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "hourlybirdinsects_hum.jpg"))


ggplot(birdinsects, aes(x = TempOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "hourlybirdinsects_temp.jpg"))


#Hourly Bird----
bird <- filter(df, RFclass == "bird") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(bird, aes(x = HumOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "hourlybirds_hum.jpg"))


ggplot(bird, aes(x = TempOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "hourlybirds_temp.jpg"))


#Monthly Insects----
insects_monthly <- filter(df, RFclass == "insect") %>% 
  filter(month != 202001 & month != 202002) %>% 
  group_by(month) %>% 
  mutate(n = n())  %>% 
  droplevels()

ggplot(insects_monthly, aes(x = NDVI_MEAN, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "monthlyinsects_ndvi.jpg"))


ggplot(insects_monthly, aes(x = TempOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "monthlyinsects_temp.jpg"))

#Hourly bird/Insects----
birdinsects_monthly <- filter(df, RFclass == "birdinsect") %>%
  filter(month != 202001 & month != 202002) %>% 
  group_by(month) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(birdinsects_monthly, aes(x = HumOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "monthlybirdinsects_hum.jpg"))


ggplot(birdinsects_monthly, aes(x = TempOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "monthlybirdinsects_temp.jpg"))


#Hourly Bird----
bird_monthly <- filter(df, RFclass == "bird") %>% 
  filter(month != 202001 & month != 202002) %>% 
  group_by(month) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(bird_monthly, aes(x = HumOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "monthlybirds_hum.jpg"))


ggplot(bird_monthly, aes(x = NDVI_MEAN, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "monthlybirds_ndvi.jpg"))



  ggplot(bird_monthly, aes(x = month, y = NDVI_MEAN)) +
  geom_smooth() +
  ggsave(getDataPath("Figures", "ndvi_month.jpg"))

df %>% filter(month != 202001 & month != 202002) %>%
  ggplot(aes(x = month, y = n)) +
  geom_smooth() +
  ggsave(getDataPath("Figures", "n_month.jpg"))

df %>% 
  ggplot(aes(x = month, y = moon_illu)) +
  geom_smooth()

