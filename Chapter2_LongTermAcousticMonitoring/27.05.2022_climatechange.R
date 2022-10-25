rm(list=ls())
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(dunn.test)
#Moon investigation

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("24.02.2022_completedf.csv"))

df$RFclass <- as.character(df$RFclass)
df$Date <- as.Date.character(df$Date) %>% 
  sort()

df$Recording_time <- factor(df$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00"))

df_2 <- group_by(df, general_category) %>% 
  summarise(n())

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

mean_temp <- df %>% group_by(month) %>% 
  mutate(monthly_temp = mean(TempOut)) %>% 
  select(monthly_temp, month) %>% 
  distinct()

historic_temp <- read.csv(getDataPath("Historic_temp_SERF.csv")) %>% 
separate(Date, into = c("year", "monthday"), sep = 4, remove = F) %>% 
  separate(monthday, into = c("month", "day"), sep = 2, remove = T) %>% 
  mutate(mean_temp = (T.Max+T.Min)/2) %>% 
  group_by(month, day) %>% 
  mutate(day_historic_temp = mean(mean_temp)) %>% 
  select(month, day_historic_temp) %>% 
  distinct()

df_complete <- df %>% 
  separate(Date, into = c("year", "month", "day"), sep = "-", remove = F) %>% 
  full_join(., historic_temp, by = c("month", "day")) %>% 
  ungroup() %>% 
  select(RFclass, month, date, TempOut, day_historic_temp) %>% 
  group_by(date) %>% 
  mutate(mean_daily_temp = mean(TempOut)) %>% 
  ungroup() %>% 
  group_by(date, RFclass) %>% 
  mutate(n = n()) %>% 
  select(date, month, RFclass, day_historic_temp, mean_daily_temp, n) %>% 
  distinct() %>% 
  mutate(climate_change = case_when(mean_daily_temp < day_historic_temp ~ "lower_temp",
                                    mean_daily_temp == day_historic_temp ~ "no_change",
                                   mean_daily_temp > day_historic_temp ~ "higher_temp")) %>% 
  distinct()



test <- df_complete %>% ungroup() %>% 
  filter(RFclass == "bird" | RFclass == "birdinsect" | RFclass == "insect") %>% 
  pivot_wider(names_from = RFclass, values_from = n)

test[is.na(test)] <- 0

kruskal.test(test$climate_change, test$bird)
kruskal.test(test$climate_change, test$insect)
kruskal.test(test$climate_change, test$birdinsect)

test <- group_by(test, climate_change, month) %>% 
  mutate(n_days = n(), 
         bird_total = sum(bird),
         insect_total = sum(insect),
         birdinsect_total = sum(birdinsect)) %>% 
  select(everything(), -c(date, insect, birdinsect, bird, mean_daily_temp)) %>% 
  distinct() %>% 
  mutate(avg_bird = bird_total/n_days,
         avg_insect = insect_total/n_days,
         avg_birdinsect = birdinsect_total/n_days)

library(lubridate)
test$date <- ymd(test$date)

  

(a <- test %>%
  ggplot(aes(x = date, y = insect, colour = climate_change)) +
  geom_point() +
  geom_smooth(se = F) +
  theme(text = element_text(size = 20), legend.position = "none") +
  labs(x = "Date", y = "Insect"))
  ggsave(getDataPath("Figures", "climate_change_insect.jpg"))
  

(b <- test %>% #filter(bird != 0) %>% 
  ggplot(aes(x = date, y = bird, colour = climate_change)) +
  geom_point() +
  geom_smooth(se = F) +
    theme(text = element_text(size = 20), legend.position = "none") +
    labs(x = "Date", y = "Bird"))
  ggsave(getDataPath("Figures", "climate_change_bird.jpg"))

c <- test %>% 
  ggplot(aes(x = date, y = birdinsect, colour = climate_change)) +
  geom_point() +
  geom_smooth(se = F) +
  theme(text = element_text(size = 20), legend.position = "none") +
  labs(x = "Date", y = "Bird/insect") +
  scale_colour_discrete(name = "Temperature in relation to historic mean", labels = c("Higher than average", "Lower than average"))
ggsave(getDataPath("Figures", "climate_change_birdinsect.jpg"))

library(cowplot)

p1 <- plot_grid(a,b, labels = c("A", "B"))
plot_grid(p1, c, nrow = 2, labels = c(" ", "C"), rel_widths = c(2,1))
ggsave(getDataPath("Figures", "GoodFigs", "08.07.2022_fig7.tiff"))


  

  
         
         