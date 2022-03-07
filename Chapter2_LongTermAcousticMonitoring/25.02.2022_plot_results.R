library(tidyverse)
library(ggplot2)
library(markdown)
library(randomForest)
library(scales)



rm(list = ls())

set.seed(123)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("24.02.2022_completedf.csv"))

plot_df <- filter(df, general_category == "anthrophony/biophony" | general_category == "anthrophony/biophony/geophony" | general_category == "biophony" | general_category == "biophony/geophony") %>% 
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
                                TRUE ~ as.character(RFclass)
  )) %>% 
  droplevels(.)

plot_df$RFclass <- as.factor(plot_df$RFclass)
plot_df$period <- as.factor(plot_df$period)
plot_df$Date <- as.Date.character(plot_df$Date) %>% 
  sort()

plot_df$Recording_time <- factor(plot_df$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00"))

#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

# Hourly plots

ggplot(data = plot_df, aes(x = as.factor(Recording_time), fill = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("#c51b7d", "#9ebcda", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  labs(fill = "Sound class", x = "Time", y = "% sound class") +
  coord_polar() +
  facet_wrap(.~month) +
  ggsave(getDataPath("Figures", "25.02.2022_RosePlot_hourly.jpg"), width = 15, height = 9)


### Daily data----
library(scales)


data <- #filter(plot_df, RFclass == "bird") %>% 
  select(plot_df, RFclass, period, Date, moon_illu, TempOut, HumOut, Rain) %>% 
  na.exclude() %>%
  group_by(., RFclass, period, Date, moon_illu, TempOut, HumOut, Rain) %>%
  mutate(n = n()) %>%
  group_by(period, Date) %>%
  mutate(n_group = n()) %>%
  distinct() %>%
  mutate(proportion = (n/n_group)*100) %>%
  group_by(Date) %>% 
  mutate(label = cur_group_id()) %>% 
  select(RFclass, period, Date, label, TempOut, HumOut, Rain, proportion, moon_illu, everything()) %>% 
  # pivot_longer(., cols = c(proportion, TempOut, HumOut, moon_illu), names_to = "variable_name", values_to = "values") %>%
  droplevels() %>%
  # mutate(scaled_values = scale(values))
  #mutate_at(c(5:9),funs(c(scale(., center = F)))) %>% 
  group_by(Date) %>% 
  mutate(n_date = n(),
         mean_temp = mean(TempOut),
         sd_temp = sd(TempOut),
         se_temp = sd_temp/sqrt(n_date),
         ci_temp = qt(0.975, df = n_date -1) * sd_temp/sqrt(n_date),
         mean_hum = mean(HumOut),
         sd_hum = sd(HumOut),
         se_hum = sd_hum/sqrt(n_date),
         ci_hum = qt(0.975, df = n_date -1) * sd_hum/sqrt(n_date))
         # mean_moon = mean(moon_illu),
         # sd_moon = sd(moon_illu),
         # se_moon = sd_moon/sqrt(n_date),
         # ci_moon = qt(0.975, df = n_date -1) * sd_moon/sqrt(n_date))

#Temperature and birds insect daily ----
data %>% filter(RFclass == "bird" | RFclass == "birdinsect" | RFclass == "insect") %>% 
  ggplot(., aes(x=sort(Date))) + 
  geom_bar(aes(fill = RFclass)) +
  #geom_density(adjust = 1/3, aes(fill = RFclass))+
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac"))+
  #geom_point(aes(y = mean_temp/1500)) +
  geom_line(aes(y = mean_temp, colour = mean_temp))+
  # geom_errorbar(aes(ymin = mean_temp/1500 - se_temp/1500, ymax = mean_temp/1500 + se_temp/1500))+
  geom_smooth(aes(y = mean_temp), colour = "black") +
  #scale_colour_manual(values = "#000000") +
  scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
  scale_colour_gradientn(colours = c("#fdcc8a", "#fc8d59", "#d7301f"))+
  theme(axis.text.y = element_blank()) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  facet_wrap(~RFclass)+
 ggsave(getDataPath("Figures", "25.02.2022_biod_temp_daily_barplot.jpg"), height = 7, width = 15)
  #geom_smooth(aes(x))
  

#Humidity and birds daily ----

data %>% filter(RFclass == "bird") %>% 
  ggplot(., aes(x=sort(Date))) +
  geom_bar(aes(fill = RFclass)) +
  #geom_density(adjust = 1/2, aes(fill = RFclass))+
  #geom_point(aes(y = mean_temp/1500)) +
  geom_line(aes(y = mean_hum/2, colour = mean_hum))+
  #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
  geom_smooth(aes(y = mean_hum/2), colour = "black") +
  scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9"))+
  scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
  theme(axis.text.y = element_blank()) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  facet_wrap(~RFclass)+
  ggsave(getDataPath("Figures", "25.02.2022_birdhum_daily_barplot.jpg"), height = 7, width = 10)

#Humidity and birdinsect daily ----

data %>% filter(RFclass == "birdinsect") %>% 
  ggplot(., aes(x=sort(Date))) + 
  geom_bar(aes(fill= RFclass)) +
  #geom_density(adjust = 1/2, aes(fill = RFclass))+
  #geom_point(aes(y = mean_temp/1500)) +
  geom_line(aes(y = mean_hum/4, colour = mean_hum))+
  #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
  geom_smooth(aes(y = mean_hum/4), colour = "black") +
  scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
  scale_fill_manual(values = c("#e9a3c9"))+
  scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
  theme(axis.text.y = element_blank()) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  facet_wrap(~RFclass) +
  ggsave(getDataPath("Figures", "25.02.2022_birdinsecthum_daily_barplot.jpg"), height = 7, width = 10)
 
 data %>% filter(RFclass == "insect") %>% 
   ggplot(., aes(x=sort(Date))) + 
   geom_bar(aes(fill = RFclass)) +
   #geom_density(adjust = 1/6, aes(fill = RFclass))+
   #geom_point(aes(y = mean_temp/1500)) +
   geom_line(aes(y = mean_hum/3, colour = mean_hum))+
   #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
   geom_smooth(aes(y = mean_hum/3), colour = "black") +
   scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
   scale_fill_manual(values = c("#5ab4ac"))+
   scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
   theme(axis.text.y = element_blank()) +
   labs(fill = "Sound class", x = "time", y = "% sound class per period") +
   facet_wrap(~RFclass) +
   ggsave(getDataPath("Figures", "25.02.2022_insecthum_daily_barplot.jpg"), height = 7, width = 10)

#Moon and bird insect daily ----

data %>% filter(RFclass == "birdinsect") %>% 
  ggplot(., aes(x=sort(Date))) + 
   geom_bar(aes(fill = RFclass)) +
  #geom_density(adjust = 1/2, aes(fill = RFclass))+
  #geom_point(aes(y = mean_temp/1500)) +
  geom_line(aes(y = moon_illu*, colour = moon_illu))+
  #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
  geom_smooth(aes(y = moon_illu*100), colour = "black") +
  scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
  scale_fill_manual(values = c("#e9a3c9"))+
  scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
  theme(axis.text.y = element_blank()) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  facet_wrap(~RFclass)
  ggsave(getDataPath("Figures", "25.02.2022_birdinsectmoon_daily_barplot.jpg"), height = 7, width = 10)
 
#moon and insect daily
 
 data %>% filter(RFclass == "insect") %>% 
   ggplot(., aes(x=sort(Date))) + 
   geom_bar(aes(fill = RFclass)) +
   #geom_density(adjust = 1/3, aes(fill = RFclass))+
   #geom_point(aes(y = moon_illu*50)) +
   geom_line(aes(y = moon_illu*50, colour = moon_illu))+
   #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
   #geom_smooth(aes(y = moon_illu*50), colour = "black") +
   scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
   scale_fill_manual(values = c("#e9a3c9"))+
   scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
   theme(axis.text.y = element_blank()) +
   labs(fill = "Sound class", x = "time", y = "% sound class per period") +
   facet_wrap(~RFclass) 
   ggsave(getDataPath("Figures", "25.02.2022_insectmoon_daily.jpg"), height = 7, width = 10)

#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda
#temp #feb24c
 
data <- #filter(plot_df, RFclass == "bird") %>% 
   select(plot_df, RFclass, period, month, TempOut, HumOut, EBI_RANGE, NDVI_MEAN) %>% 
   na.exclude() %>%
   group_by(., RFclass, month, TempOut, HumOut, EBI_RANGE, NDVI_MEAN) %>%
   mutate(n = n()) %>%
   group_by(period, month) %>%
   mutate(n_group = n()) %>%
   distinct() %>%
   mutate(proportion = (n/n_group)*100) %>%
   group_by(Date) %>% 
   mutate(label = cur_group_id()) %>% 
   select(RFclass, period, month, label, TempOut, HumOut, EBI_RANGE, NDVI_MEAN, proportion, everything()) %>% 
   # pivot_longer(., cols = c(proportion, TempOut, HumOut, moon_illu), names_to = "variable_name", values_to = "values") %>%
   droplevels() %>%
   # mutate(scaled_values = scale(values))
   #mutate_at(c(5:9),funs(c(scale(., center = F)))) %>% 
   group_by(Date) %>% 
   mutate(n_date = n(),
          mean_temp = mean(TempOut),
          sd_temp = sd(TempOut),
          se_temp = sd_temp/sqrt(n_date),
          ci_temp = qt(0.975, df = n_date -1) * sd_temp/sqrt(n_date),
          mean_hum = mean(HumOut),
          sd_hum = sd(HumOut),
          se_hum = sd_hum/sqrt(n_date),
          ci_hum = qt(0.975, df = n_date -1) * sd_hum/sqrt(n_date))
 
 
 data %>% filter(RFclass == "insect" & moon_illu != 0) %>% 
   ggplot(., aes(x=sort(Date))) + 
   geom_density(adjust = 1/3, aes(fill = RFclass))+
   #geom_point(aes(y = mean_temp/1500)) +
   #geom_line(aes(y = moon_illu/10, colour = moon_illu))+
   #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
   geom_smooth(aes(y = moon_illu/50), colour = "black") +
   scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
   scale_fill_manual(values = c("#e9a3c9"))+
   scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
   theme(axis.text.y = element_blank()) +
   labs(fill = "Sound class", x = "time", y = "% sound class per period") +
   facet_wrap(~RFclass) +
   ggsave(getDataPath("Figures", "25.02.2022_insectmoon_daily.jpg"), height = 7, width = 10)
 
 
 #Monthly plots ----
 data_month <- #filter(plot_df, RFclass == "bird") %>% 
   select(plot_df, RFclass, period, month, TempOut, HumOut, EBI_RANGE, NDVI_MEAN) %>% 
   na.exclude() %>%
   group_by(., RFclass, month, TempOut, HumOut, EBI_RANGE, NDVI_MEAN) %>%
   mutate(n = n()) %>%
   group_by(month) %>%
   mutate(n_group = n()) %>%
   distinct() %>%
   mutate(proportion = (n/n_group)*100) %>%
   group_by(month) %>% 
   mutate(label = cur_group_id()) %>% 
   select(RFclass, period, month, label, TempOut, HumOut, EBI_RANGE, NDVI_MEAN, proportion, everything()) %>% 
   # pivot_longer(., cols = c(proportion, TempOut, HumOut, moon_illu), names_to = "variable_name", values_to = "values") %>%
   droplevels() %>%
   # mutate(scaled_values = scale(values))
   #mutate_at(c(5:9),funs(c(scale(., center = F)))) %>% 
   group_by(month) %>% 
   mutate(n_month = n(),
          mean_temp = mean(TempOut),
          sd_temp = sd(TempOut),
          se_temp = sd_temp/sqrt(n_month),
          ci_temp = qt(0.975, df = n_month -1) * sd_temp/sqrt(n_month),
          mean_hum = mean(HumOut),
          sd_hum = sd(HumOut),
          se_hum = sd_hum/sqrt(n_month),
          ci_hum = qt(0.975, df = n_month -1) * sd_hum/sqrt(n_month),
          mean_ebi = mean(EBI_RANGE),
          sd_ebi = sd(EBI_RANGE),
          se_ebi = sd_ebi/sqrt(n_month),
          ci_ebi = qt(0.975, df = n_month -1) * sd_ebi/sqrt(n_month),
          mean_ndvi = mean(NDVI_MEAN),
          sd_ndvi = sd(NDVI_MEAN),
          se_ndvi = sd_ndvi/sqrt(n_month),
          ci_ndvi = qt(0.975, df = n_month -1) * sd_ndvi/sqrt(n_month)) %>% 
   separate(month, into = c("year", "month", ))
 
 data_month$month <- as.factor(data_month$month)
 
 data %>% filter(RFclass == "bird") %>% 
   ggplot(., aes(x=sort(month))) + 
   geom_bar(aes(fill = RFclass))
   geom_bar(aes(fill = RFclass, y = proportion), stat = "identity")
   #geom_density(adjust = 1/2, aes(fill = RFclass))+
   #geom_point(aes(y = mean_temp/1500)) +
   #geom_line(aes(y = moon_illu/10, colour = moon_illu))+
   #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
   geom_smooth(aes(y = mean_hum/200), colour = "black") +
   #scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
   scale_fill_manual(values = c("#e9a3c9"))+
   scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
   theme(axis.text.y = element_blank()) +
   labs(fill = "Sound class", x = "time", y = "% sound class per period") +
   facet_wrap(~RFclass) +
   ggsave(getDataPath("Figures", "25.02.2022_insectmoon_daily.jpg"), height = 7, width = 10)

