rm(list=ls())
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(dunn.test)
#Moon investigation

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
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

heatmap <- select(df, Date, Recording_time, n_rec, RFclass) %>% 
  group_by(Date, Recording_time, RFclass) %>% 
  summarise(n_rec = sum(n_rec))

heatmap <- select(df, Date, Recording_time, n_rec) %>% 
  group_by(Date, Recording_time) %>% 
  summarise(n_rec = sum(n_rec))

heatmap_birds <- heatmap %>% 
  #filter(RFclass == "birdinsect") %>% 
  pivot_wider(names_from = Recording_time, values_from = n_rec, values_fill = 0) %>% 
  select("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00") %>% 
  ungroup()

heatmap_birds <- column_to_rownames(heatmap_birds, var = "Date")

heatmap_birds <- as.matrix(heatmap_birds)

heatmap_b <- heatmap(heatmap_birds, Rowv = NA, Colv = NA)

tiff(file = getDataPath("Figures", "GoodFigs", "22.01.2023_heatmap_overall.tiff"))
heatmap(heatmap_birds, Rowv = NA, Colv = NA)
dev.off()

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

library(rstatix)

kruskal.test(test$climate_change, test$bird)
kruskal_effect <- test %>% kruskal_effsize(test$bird ~ test$climate_change)
write.csv(kruskal_effect, getDataPath("kruskal_effect_bird.csv"))


kruskal.test(test$climate_change, test$insect)
kruskal_effect_insect <- test %>% kruskal_effsize(test$insect ~ test$climate_change)
write.csv(kruskal_effect_insect, getDataPath("kruskal_effect_insect.csv"))

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
  # scale_x_continuous(labels = c("Jan 2020" = "Jan", "Apr 2020" = "Apr", "Jul 2020" = "Jul", "Oct 2020" = "Oct", "Jan 2021" = "Jan2021")))
  ggsave(getDataPath("Figures", "22.01.2023_climate_change_insect.jpg"))
  

(b <- test %>% filter(period == day) %>% 
  ggplot(aes(x = date, y = bird, colour = climate_change)) +
  geom_point() +
  geom_smooth(se = F) +
    theme(text = element_text(size = 20), legend.position = "none") +
    labs(x = "Date", y = "Bird")) 
  ggsave(getDataPath("Figures", "22.01.2023_climate_change_bird.jpg"))

(c <- test %>% 
  ggplot(aes(x = date, y = birdinsect, colour = climate_change)) +
  geom_point() +
  geom_smooth(se = F) +
  theme(text = element_text(size = 20), legend.position = "none") +
  labs(x = "Date", y = "Bird/insect") +
  scale_colour_discrete(name = "Temperature in relation to historic mean", labels = c("Higher than average", "Lower than average")))
ggsave(getDataPath("Figures", "22.01.2023_climate_change_birdinsect.jpg"))

library(cowplot)

p1 <- plot_grid(a,b, labels = c("A", "B"))

plot <- plot_grid(p1, c, nrow = 2, labels = c(" ", "C"), rel_widths = c(1,2))
save_plot(getDataPath("Figures", "GoodFigs", "22.01.2023_fig7.tiff"), plot, base_height = 8.5, base_width = 12.5)


  

  
         
         