library(tidyverse)
library(ggplot2)
#Moon investigation

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("24.02.2022_completedf.csv"))

df$RFclass <- as.character(df$RFclass)

df$Recording_time <- factor(df$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00"))

df <- filter(df, general_category == "anthrophony/biophony" | general_category == "anthrophony/biophony/geophony" | general_category == "biophony" | general_category == "biophony/geophony") %>% 
  mutate(., general_category = "biophony") %>% 
  mutate(., moon_illu = case_when(period == "day" ~ 0,
                                  TRUE ~ moon_illu)) %>% 
  mutate(., RFclass = case_when(RFclass == "anthrobird" ~ "bird",
                                RFclass == "anthrobirdfroggeoinsect" ~ "birdfroginsect",
                                RFclass == "anthrobirdgeo" ~ "bird",
                                RFclass == "anthrobirdgeoinsect" ~ "birdinsect",
                                RFclass == "anthrobirdinsect" ~ "birdinsect",
                                RFclass == "anthrofroggeoinsect" ~ "froginsect",
                                RFclass == "anthrofroginsect" ~ "froginsect",
                                RFclass == "anthrogeoinsect" ~ "insect",
                                RFclass == "anthroinsect" ~ "insect",
                                RFclass == "birdgeo" ~ "bird",
                                RFclass == "birdgeoinsect" ~ "birdinsect",
                                RFclass == "froggeoinsect" ~ "froginsect",
                                RFclass == "geoinsect" ~ "insect",
                                TRUE ~ as.character(RFclass))) %>% 
  group_by(Date, Recording_time) %>% 
  mutate(n_rec = n(),
         mean_temp = mean(TempOut),
         sd_temp = sd(TempOut),
         se_temp = sd_temp/sqrt(n_rec),
         ci_temp = qt(0.975, df = n_rec -1) * sd_temp/sqrt(n_rec),
         mean_hum = mean(HumOut),
         sd_hum = sd(HumOut),
         se_hum = sd_hum/sqrt(n_rec),
         ci_hum = qt(0.975, df = n_rec -1) * sd_hum/sqrt(n_rec)) %>% 
    droplevels(.)

#Hourly Insects ----
insects <- filter(df, RFclass == "insect") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(insects, aes(x = HumOut, y = n)) + 
  geom_smooth( ) +
  ggsave(getDataPath("Figures", "08.03.2022_hourlyinsects_humidity.jpg"))

df %>% mutate(RFclass = na_if(RFclass, "bird"),
              RFclass = na_if(RFclass, "birdfroginsect"),
              RFclass = na_if(RFclass, "birdinsect"),
              RFclass = na_if(RFclass, "froginsect")) %>%
ggplot(., aes(x = Recording_time)) +
  geom_line(aes(y = mean_hum, colour = mean_hum)) +
  geom_bar(aes(fill = RFclass), na.rm = T) +
  facet_wrap(~month)

ggplot(insects, aes(x = sort(Recording_time))) +
  geom_bar() +
  geom_line(aes(y= mean_hum, colour = mean_hum)) +
  #geom_point(aes(y = mean_hum, colour = mean_hum)) +
  #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
  scale_x_discrete(labels = c("0:00", "2:00", "4:00", "6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00")) +
  facet_wrap(~month)
  ggsave(getDataPath("Figures", "08.03.2022_hourlyinsects_humidity.jpg"))


ggplot(insects, aes(x = moon_illu, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_hourlyinsects_moon.jpg"))

#Hourly bird/Insects----
birdinsects <- filter(df, RFclass == "birdinsect") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(birdinsects, aes(x = HumOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_hourlybirdinsects_hum.jpg"))


ggplot(birdinsects, aes(x = TempOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_hourlybirdinsects_temp.jpg"))


#Hourly Bird----
bird <- filter(df, RFclass == "bird") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(bird, aes(x = HumOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_hourlybirds_hum.jpg"))


ggplot(bird, aes(x = TempOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_hourlybirds_temp.jpg"))


#Monthly Insects----
insects_monthly <- filter(df, RFclass == "insect") %>% 
  filter(month != 202001 & month != 202002) %>% 
  group_by(month) %>% 
  mutate(n = n())  %>% 
  droplevels()

ggplot(insects_monthly, aes(x = NDVI_MEAN, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_monthlyinsects_ndvi.jpg"))


ggplot(insects_monthly, aes(x = TempOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_monthlyinsects_temp.jpg"))

#Hourly bird/Insects----
birdinsects_monthly <- filter(df, RFclass == "birdinsect") %>%
  filter(month != 202001 & month != 202002) %>% 
  group_by(month) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(birdinsects_monthly, aes(x = HumOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_monthlybirdinsects_hum.jpg"))


ggplot(birdinsects_monthly, aes(x = TempOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_monthlybirdinsects_temp.jpg"))


#Hourly Bird----
bird_monthly <- filter(df, RFclass == "bird") %>% 
  filter(month != 202001 & month != 202002) %>% 
  group_by(month) %>% 
  mutate(n = n()) %>% 
  droplevels()

ggplot(bird_monthly, aes(x = HumOut, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_monthlybirds_hum.jpg"))


ggplot(bird_monthly, aes(x = NDVI_MEAN, y = n)) + 
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_monthlybirds_ndvi.jpg"))



  ggplot(bird_monthly, aes(x = month, y = NDVI_MEAN)) +
  geom_smooth() +
  ggsave(getDataPath("Figures", "08.03.2022_ndvi_month.jpg"))

df %>% filter(month != 202001 & month != 202002) %>%
  ggplot(aes(x = month, y = n)) +
  geom_smooth() +
  ggsave(getDataPath("Figures", "n_month.jpg"))

df %>% 
  ggplot(aes(x = month, y = moon_illu)) +
  geom_smooth()

