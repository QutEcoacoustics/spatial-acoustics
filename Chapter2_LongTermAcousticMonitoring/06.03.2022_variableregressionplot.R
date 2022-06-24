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


df$RFclass <- as.factor(df$RFclass)
df$period <- as.factor(df$period)
df$Date <- as.Date.character(df$Date) %>% 
  sort()

df$Recording_time <- factor(plot_df$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00"))


plot_monthly <- select(df, month, RFclass, NDVI_MEAN, HumOut, TempOut) %>% 
  na.exclude(.) %>% 
  group_by(., month, RFclass) %>% 
  summarise(count_class = n(),
            ndvi = mean(NDVI_MEAN),
            Humidity = mean(HumOut),
            Temp = mean(TempOut)) %>% 
  pivot_longer(., cols = c(ndvi, count_class, Humidity, Temp), values_to = "mean", names_to = "variables") %>% 
  mutate(scaled = scale(mean)) %>% 
  mutate(., month = case_when(#month == as.character("202001") ~ as.Date.character("2020-01-01"),
    #month == as.character("202002") ~ as.Date.character("2020-02-01"),
    month == as.character("202003") ~ as.character("2020-03-01"),
    month == as.character("202004") ~ as.character("2020-04-01"),
    month == as.character("202005") ~ as.character("2020-05-01"),
    month == as.character("202006") ~ as.character("2020-06-01"),
    month == as.character("202007") ~ as.character("2020-07-01"),
    month == as.character("202008") ~ as.character("2020-08-01"),
    month == as.character("202009") ~ as.character("2020-09-01"),
    month == as.character("202010") ~ as.character("2020-10-01"),
    month == as.character("202011") ~ as.character("2020-11-01"),
    month == as.character("202012") ~ as.character("2020-12-01"), 
    TRUE ~ as.character(month)))

plot_monthly$month <- as.Date.character(plot_monthly$month)

#Night plot ----

period_test <- "night" 

data <- filter(df, period == period_test) %>% 
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony, Recording_time, time2, n_days) %>% 
  na.exclude() %>% 
  group_by(month) %>% 
  mutate(n_motif =  n()) %>%
  droplevels()

test <- df %>%
  group_by(Date, RFclass) %>%
  mutate(n_class = n()) %>% 
  droplevels(.)

test <- df %>%
  group_by(Date, RFclass) %>%
  mutate(n = n())


bird <- filter(test, RFclass == "bird") %>%   
        kruskal.test(x = .$n, g = .$month) 

filter(test, RFclass == "bird") %>% 
  dunn.test(x = .$n, g = .$month)


insect <- filter(test, RFclass == "insect") %>%   
  kruskal.test(x = .$n, g = .$month)

filter(test, RFclass == "insect") %>% 
  dunn.test(x = .$n, g = .$month)

birdinsect <- filter(test, RFclass == "birdinsect") %>%   
  kruskal.test(x = .$n, g = .$month)

filter(test, RFclass == "birdinsect") %>% 
  dunn.test(x = .$n, g = .$month)
  

labels <- select(data, month, n_motif, n_days) %>% 
  distinct() %>% 
  .[order(.$month),] %>% 
  mutate(month = case_when(month == "202001" ~ "Jan",
                           month == "202002" ~ "Feb",
                           month == "202003" ~ "Mar",
                           month == "202004" ~ "Apr",
                           month == "202005" ~ "May",
                           month == "202006" ~ "Jun",
                           month == "202007" ~ "Jul",
                           month == "202008" ~ "Aug",
                           month == "202009" ~ "Sep",
                           month == "202010" ~ "Oct",
                           month == "202011" ~ "Nov",
                           month == "202012" ~ "Dec")) #%>% 
  # mutate(average_motif = paste("Motif/day = ", format(round(n_motif/n_days, 1)), sep = "")) %>% 
  # mutate(labels = paste(month, average_motif, sep = "\n"))

### Rose plot - monthly biod
(p_night <- ggplot(data = data, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..))) +
  scale_fill_manual(values = c("bird" = "#c51b7d", "birdinsect" = "#e9a3c9", "insect" = "#5ab4ac", "froginsect" = "#4d9221", "birdfroginsect" = "#9ebcda")) +
  labs(fill = "Sound class", x = "Month", y = "Sound class count per period/month", caption = paste("Recording hours (6): ",levels(data$Recording_time)[4], ", ", levels(data$Recording_time)[5], ", ", levels(data$Recording_time)[6], ", ", levels(data$Recording_time)[1], ", ", levels(data$Recording_time)[2], ", ", levels(data$Recording_time)[3], sep = "")) +
  scale_x_discrete(labels = labels$month, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_polar() +
  theme_light(base_size = 15))
  ggsave(getDataPath("Figures", "GoodFigs", paste("23.06.2022_RosePlot_", period_test, ".tiff", sep = "")), width = 12, height = 10, scale = 1)
  
period_test <- "dusk" 

data <- filter(df, period == period_test) %>% 
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony, Recording_time, time2, n_days) %>% 
  na.exclude() %>% 
  group_by(month) %>% 
  mutate(n_motif =  n()) %>%
  droplevels()


(p_dusk <- ggplot(data = data, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..))) +
  scale_fill_manual(values = c("bird" = "#c51b7d", "birdinsect" = "#e9a3c9", "insect" = "#5ab4ac", "froginsect" = "#4d9221", "birdfroginsect" = "#9ebcda")) +
  labs(fill = "Sound class", x = "Month", y = "Sound class count per period/month", caption = paste("Recording hours (3): ", levels(data$Recording_time)[1], ", ", levels(data$Recording_time)[2], ", ", levels(data$Recording_time)[3], sep = "")) +
  scale_x_discrete(labels = labels$month, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_polar() +
  theme_light(base_size = 15))
ggsave(getDataPath("Figures", "GoodFigs", paste("23.06.2022_RosePlot_", period_test, ".tiff", sep = "")), width = 12, height = 10, scale = 1)

  
(cow_dusk_night <- plot_grid(p_dusk, p_night, labels = c("A", "B"), scale = 1)), rel_widths = c(2,2), rel_heights = c(1,1), label_size = 10))

(cow1 <- plot_grid(p1, labels = c("A"), scale = 1, rel_widths = c(2,2), rel_heights = c(1,1), label_size = 10))
(cow2 <- plot_grid(p2, p3, p4, labels = c("B", "C", "D"), rel_widths = c(2,2), rel_heights = c(1,1), label_size = 10, ncol = 3))
(cow3 <- plot_grid(cow1, cow2, nrow = 2, rel_heights = c(2,1)))
save_plot(getDataPath("Figures", "GoodFigs", "07.06.2022_cow_insects.jpg"), cow3, base_height = 8.5, base_width = 15.5)


#Hourly Insects ----
insects <- filter(df, RFclass == "insect") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

p1 <- plot_monthly %>% filter(RFclass == "insect" & variables != "Humidity") %>% 
  droplevels() %>% 
  #mutate(scaled = scale(mean)) %>% 
  ggplot(., aes(x=month, y = scaled, colour = variables)) +
  geom_smooth() +
  scale_colour_manual(values = c("#5ab4ac", "#238b45", "#fd8d3c"), labels = c("Insect", "NDVI", "Temperature")) +
  labs(colour = "Variables", x = "Month", y = "Scaled values") +
  scale_x_date(breaks = "2 month", labels = c("February", "April", "June", "August", "October", "December", "Januray")) +
  theme_light() 
  ggsave(getDataPath("Figures", "GoodFigs", "18.03.2022_insectmonthvars.jpg"))

p2 <- ggplot(insects, aes(x = HumOut, y = n)) + 
  geom_smooth(colour = "black") +
  theme_light() +
  labs(x = "Humidity", y = "Number of motifs")
  ggsave(getDataPath("Figures", "GoodFigs", "20.03.2022_hourlyinsects_humidity.jpg"))

# ggplot(insects, aes(x = Recording_time, na.rm = T)) +
#   geom_bar(aes(fill = RFclass), na.rm = T) +
#   geom_line(data = df, aes(y = mean_hum, colour = mean_hum)) +
#   facet_wrap(~month) +
#   ggsave(getDataPath("Figures", "09.03.2022_hourlyinsects_humidity.jpg"))

# ggplot(insects, aes(x = Recording_time, na.rm = T)) +
#   geom_bar(aes(fill = RFclass), na.rm = T) +
#   geom_line(data = df, aes(y = mean_hum, colour = mean_hum)) +
#   #geom_point(aes(y = mean_hum, colour = mean_hum)) +
#   #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
#   scale_x_discrete(labels = c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22")) +
#   scale_fill_manual(values = "#5ab4ac") +
#   scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe")) +
#   facet_wrap(~month) +
#   ggsave(getDataPath("Figures", "09.03.2022_hourlyinsects_humidity.jpg"), height = 9, width = 16)
  
# library(ggforce)
# 
# ggplot(insects, aes(x = Recording_time, na.rm = T)) +
#   geom_bar(aes(fill = RFclass), na.rm = T) +
#   geom_line(data = df, aes(y = moon_illu*100, colour = moon_illu)) +
#   #geom_point(aes(y = mean_hum, colour = mean_hum)) +
#   #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
#   scale_x_discrete(labels = c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22")) +
#   scale_colour_gradientn(colours = c("#969696", "#636363", "#252525"))+
#   scale_fill_manual(values = "#5ab4ac")+
#   facet_wrap_paginate(~Date, ncol = 6, nrow = 5, page = 5)
#   ggsave(getDataPath("Figures", "09.03.2022_hourlyinsects_moon.jpg"), height = 9, width = 16)
  
#   
#   ggplot(insects, aes(x = Recording_time, na.rm = T)) +
#     geom_bar(aes(fill = RFclass), na.rm = T) +
#     geom_line(data = df, aes(y = moon_illu*100, colour = moon_illu)) +
#     #geom_point(aes(y = mean_hum, colour = mean_hum)) +
#     #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
#     scale_x_discrete(labels = c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22")) +
#     scale_colour_gradientn(colours = c("#969696", "#636363", "#252525"))+
#     scale_fill_manual(values = "#5ab4ac")+
#     facet_wrap(~month) +
#   ggsave(getDataPath("Figures", "09.03.2022_hourlyinsects_moon.jpg"), height = 9, width = 16)
# 
# 
# ggplot(insects, aes(x=sort(Recording_time))) + 
#   geom_bar(aes(fill = RFclass))+
#   #geom_point(data = df, aes(y = moon_illu/50)) +
#   geom_line(data = df, aes(y = moon_illu*505, colour = moon_illu))+
#   #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
#   #geom_smooth(data = df, aes(y = moon_illu/50), colour = "black") +
#   #scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
#   scale_fill_manual(values = c("#e9a3c9"))+
#   scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
#   #theme(axis.text.y = element_blank()) +
#   labs(fill = "Sound class", x = "time", y = "% sound class per period") +
#   facet_wrap(~month) 
#   ggsave(getDataPath("Figures", "09.03.2022_insectmoon_daily.jpg"), height = 7, width = 10)
#   
#   
#   ggplot(df, aes(x=sort(Date))) + 
#     #geom_density(adjust = 1/2, aes(fill = RFclass))+
#     #geom_point(data = df, aes(y = moon_illu/50)) +
#     geom_line(aes(y = moon_illu/50, colour = moon_illu))+
#     #geom_errorbar(aes(ymin = mean_hum/15000 - se_hum/15000, ymax = mean_hum/15000 + se_hum/15000))+
#     #geom_smooth(aes(y = moon_illu), colour = "black") +
#     scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
#     #scale_fill_manual(values = c("#e9a3c9"))+
#     scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe"))+
#     #theme(axis.text.y = element_blank()) +
#     #labs(fill = "Sound class", x = "time", y = "% sound class per period") +
#     facet_wrap(~month) 
#   ggsave(getDataPath("Figures", "09.03.2022_insectmoon_daily.jpg"), height = 7, width = 10)

p3 <- ggplot(insects, aes(x = moon_illu, y = n)) + 
  geom_smooth(colour = "black") +
  theme_light() +
  labs(x = "Moon illumination", y = "Number of motifs")
  ggsave(getDataPath("Figures", "GoodFigs", "20.03.2022_hourlyinsects_moon.jpg"))
  
(p4 <- ggplot(insects, aes(x = TempOut, y = n)) + 
    geom_smooth(colour = "black") +
    theme_light() +
    labs(x = "Temperature", y = "Number of motifs"))
  ggsave(getDataPath("Figures", "GoodFigs", "20.03.2022_hourlyinsects_temp.jpg"))
  
library(cowplot)
  
(cow1 <- plot_grid(p1, labels = c("A"), scale = 1, rel_widths = c(2,2), rel_heights = c(1,1), label_size = 10))
(cow2 <- plot_grid(p2, p3, p4, labels = c("B", "C", "D"), rel_widths = c(2,2), rel_heights = c(1,1), label_size = 10, ncol = 3))
(cow3 <- plot_grid(cow1, cow2, nrow = 2, rel_heights = c(2,1)))
save_plot(getDataPath("Figures", "GoodFigs", "07.06.2022_cow_insects.jpg"), cow3, base_height = 8.5, base_width = 15.5)

#Hourly bird/Insects----
birdinsects <- filter(df, RFclass == "birdinsect") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

period_test <- "dawn" 

### Daily data----

data <- filter(df, period == period_test) %>% 
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony, Recording_time, time2, n_days) %>% 
  na.exclude() %>% 
  group_by(month) %>% 
  mutate(n_motif =  n()) %>%
  droplevels()


p_dawn <- ggplot(data = data, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..))) +
  scale_fill_manual(values = c("bird" = "#c51b7d", "birdinsect" = "#e9a3c9", "insect" = "#5ab4ac", "froginsect" = "#4d9221", "birdfroginsect" = "#9ebcda")) +
  labs(fill = "Sound class", x = "Month", y = "Sound class count per period/month") +
  scale_x_discrete(labels = labels$month, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_polar() +
  theme_light(base_size = 15)
ggsave(getDataPath("Figures", "GoodFigs", paste("07.06.2022_RosePlot_", period_test, ".jpg", sep = "")), width = 12, height = 10, scale = 1)


#InsectBird

plot_monthly_insectbird <- select(df, month, RFclass, HumOut, TempOut) %>% 
  na.exclude(.) %>% 
  group_by(., month, RFclass) %>% 
  summarise(count_class = n(),
            Humidity = mean(HumOut)*10,
            Temp = mean(TempOut)*10) %>% 
  pivot_longer(., cols = c(count_class, Humidity, Temp), values_to = "mean", names_to = "variables") %>% 
  mutate(scaled = scale(mean)) %>%
  mutate(., month = case_when(month == as.character("202001") ~ as.character("2020-01-01"),
                              month == as.character("202002") ~ as.character("2020-02-01"),
                              month == as.character("202003") ~ as.character("2020-03-01"),
                              month == as.character("202004") ~ as.character("2020-04-01"),
                              month == as.character("202005") ~ as.character("2020-05-01"),
                              month == as.character("202006") ~ as.character("2020-06-01"),
                              month == as.character("202007") ~ as.character("2020-07-01"),
                              month == as.character("202008") ~ as.character("2020-08-01"),
                              month == as.character("202009") ~ as.character("2020-09-01"),
                              month == as.character("202010") ~ as.character("2020-10-01"),
                              month == as.character("202011") ~ as.character("2020-11-01"),
                              month == as.character("202012") ~ as.character("2020-12-01"), 
                              TRUE ~ as.character(month)))

plot_monthly_insectbird$month <- as.Date.character(plot_monthly_insectbird$month)

p4 <- plot_monthly_insectbird %>% filter(RFclass == "birdinsect") %>% 
  droplevels() %>% 
  ggplot(., aes(x=month, y = scaled, colour = variables)) +
  geom_smooth() +
    scale_colour_manual(values = c("#e9a3c9", "#0570b0", "#fd8d3c"), labels = c("Bird/insect", "Humidity", "Temperature")) +
  labs(colour = "Variables", x = "Month", y = "Scaled values") +
  scale_x_date(breaks = "2 month", labels = c("Januray", "February", "April", "June", "August", "October", "December", "January")) +
  theme_light() 
  ggsave(getDataPath("Figures", "GoodFigs", "18.03.2022_birdinsectmonthvars.jpg"))

p5 <- ggplot(birdinsects, aes(x = HumOut, y = n)) + 
  geom_smooth(colour = "black") +
  theme_light() +
  labs(x = "Humidity", y = "Number of motifs") 
  ggsave(getDataPath("Figures", "GoodFigs", "20.03.2022_hourlybirdinsects_hum.jpg"))

# ggplot(birdinsects, aes(x = Recording_time, na.rm = T)) +
#   geom_bar(aes(fill = RFclass), na.rm = T) +
#   geom_line(data = df, aes(y = mean_hum/2, colour = mean_hum)) +
#   #geom_point(aes(y = mean_hum, colour = mean_hum)) +
#   #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
#   scale_x_discrete(labels = c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22")) +
#   scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe")) +
#   scale_fill_manual(values = "#e9a3c9") +
#   facet_wrap(~month) +
#   ggsave(getDataPath("Figures", "09.03.2022_hourlybirdinsects_humidity.jpg"), height = 9, width = 16)


p6 <- ggplot(birdinsects, aes(x = TempOut, y = n)) + 
  geom_smooth(colour = "black") +
  theme_light() +
  labs(x = "Temperature", y = "Number of motifs")
  ggsave(getDataPath("Figures", "GoodFigs", "20.03.2022_hourlybirdinsects_temp.jpg"))

# ggplot(birdinsects, aes(x = Recording_time, na.rm = T)) +
#   geom_bar(aes(fill = RFclass), na.rm = T) +
#   geom_line(data = df, aes(y = mean_temp, colour = mean_temp)) +
#   #geom_point(aes(y = mean_hum, colour = mean_hum)) +
#   #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
#   scale_x_discrete(labels = c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22")) +
#   scale_colour_gradientn(colours = c("#fdcc8a", "#fc8d59", "#d7301f"))+
#   scale_fill_manual(values = "#e9a3c9") +
#   facet_wrap(~month) +
#   ggsave(getDataPath("Figures", "09.03.2022_hourlybirdinsects_temp.jpg"), height = 9, width = 16)
  
  #bird #c51b7d
  #birdinsect #e9a3c9
  #insect #5ab4ac
  #froginsect #4d9221
  #birdfroginsect ##9ebcda
  #temp #feb24c
cow4 <- plot_grid(p_dawn, p4, labels = c("A", "B"), label_size = 10)
cow5 <- plot_grid(p5, p6, labels = c("C", "D"), rel_widths = c(2,2), rel_heights = c(1,1), label_size = 10)
cow6 <- plot_grid(cow4, cow5, nrow = 2, rel_heights = c(2,1)) 
save_plot(getDataPath("Figures", "GoodFigs", "23.06.2022_cow_cicada.tiff"), cow6, base_height = 8.5, base_width = 10.5)

#Hourly Bird----
period_test <- "day" 

### Daily data----

data <- filter(df, period == period_test) %>% 
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony, Recording_time, time2, n_days) %>% 
  na.exclude() %>% 
  group_by(month) %>% 
  mutate(n_motif =  n()) %>%
  droplevels()

p_day <- ggplot(data = data, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..))) +
  scale_fill_manual(values = c("bird" = "#c51b7d", "birdinsect" = "#e9a3c9", "insect" = "#5ab4ac", "froginsect" = "#4d9221", "birdfroginsect" = "#9ebcda")) +
  labs(fill = "Sound class", x = "Month", y = "Sound class count per period/month") +
  scale_x_discrete(labels = labels$month, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_polar() +
  theme_light(base_size = 15)

bird <- filter(df, RFclass == "bird") %>% 
  group_by(Recording_time) %>% 
  mutate(n = n()) %>% 
  droplevels()

p7 <- plot_monthly %>% filter(RFclass == "bird" & variables != "Temp") %>%
  droplevels() %>% 
  #mutate(scaled = scale(mean)) %>% 
  ggplot(., aes(x=month, y = scaled, colour = variables)) +
  geom_smooth() +
  scale_colour_manual(values = c("#c51b7d", "#0570b0", "#238b45"), labels = c("Bird", "Humidity", "NDVI")) +
  labs(colour = "Variables", x = "Month", y = "Scaled values") +
  scale_x_date(breaks = "2 month", labels = c("February", "April", "June", "August", "October", "December", "January")) +
  theme_light()
  ggsave(getDataPath("Figures", "GoodFigs", "18.03.2022_birdmonthvars.jpg"))

# ggplot(bird, aes(x = Recording_time, na.rm = T)) +
#   geom_bar(aes(fill = RFclass), na.rm = T) +
#   geom_line(data = df, aes(y = mean_hum, colour = mean_hum)) +
#   #geom_point(aes(y = mean_hum, colour = mean_hum)) +
#   #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
#   scale_x_discrete(labels = c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22")) +
#   scale_colour_gradientn(colours = c("#ece7f2", "#a6bddb", "#2b8cbe")) +
#   scale_fill_manual(values = "#c51b7d") +
#   facet_wrap(~month) +
#   ggsave(getDataPath("Figures", "09.03.2022_hourlybird_humidity.jpg"), height = 9, width = 16)

p8 <- ggplot(bird, aes(x = HumOut, y = n)) + 
  geom_smooth(colour = "black") +
  theme_light() +
  labs(x = "Humidity", y = "Number of motifs")
  ggsave(getDataPath("Figures", "GoodFigs", "20.03.2022_hourlybird_hum.jpg"))

# 
# ggplot(bird, aes(x = Recording_time, na.rm = T)) +
#   geom_bar(aes(fill = RFclass), na.rm = T) +
#   geom_line(data = df, aes(y = mean_temp, colour = mean_temp)) +
#   #geom_point(aes(y = mean_hum, colour = mean_hum)) +
#   #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
#   scale_x_discrete(labels = c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22")) +
#   scale_colour_gradientn(colours = c("#fdcc8a", "#fc8d59", "#d7301f"))+
#   scale_fill_manual(values = "#c51b7d") +
#   facet_wrap(~month) +
#   ggsave(getDataPath("Figures", "09.03.2022_hourlybird_temp.jpg"), height = 9, width = 16)

p9 <- ggplot(bird, aes(x = TempOut, y = n)) + 
  geom_smooth(colour = "black") +
  theme_light() +
  labs(x = "Temperature", y = "Number of motifs")
  ggsave(getDataPath("Figures", "GoodFigs", "20.03.2022_hourlybird_temp.jpg"))

cow7 <- plot_grid(p_day, p7, labels = c("A", "B"), label_size = 10)
cow8 <- plot_grid(p8, p9, labels = c("B", "C"), rel_widths = c(2,2), rel_heights = c(1,1), label_size = 10)
cow9 <- plot_grid(cow7, cow8, nrow = 2, rel_heights = c(2,1)) 
save_plot(getDataPath("Figures", "GoodFigs", "23.06.2022_cow_bird.tiff"), cow9, base_height = 8.5, base_width = 10.5)


#Monthly Insects----
# insects_monthly <- filter(df, RFclass == "insect") %>% 
#   filter(month != 202001 & month != 202002) %>% 
#   group_by(month) %>% 
#   mutate(n = n(),
#          mean_ndvi = mean(NDVI_MEAN),
#          mean_ebi = mean(EBI_RANGE))  %>% 
#   droplevels()
# 
# ggplot(insects_monthly, aes(x = mean_ndvi, y = n)) + 
#   geom_smooth() +
#   ggsave(getDataPath("Figures", "08.03.2022_monthlyinsects_ndvi.jpg"))
# 
# insects_monthly$Date <- as.Date.character(insects_monthly$Date)
# 
# ggplot(insects_monthly, aes(x = sort(Date), na.rm = T)) +
#   geom_bar(aes(fill = RFclass), na.rm = T) +
#   geom_line(aes(y = mean_ndvi, colour = mean_ndvi)) +
#   scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
#   #geom_point(aes(y = mean_hum, colour = mean_hum)) +
#   #geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp)) +
#   #scale_x_discrete(labels = c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22")) +
#   #scale_colour_gradientn(colours = c("#fdcc8a", "#fc8d59", "#d7301f"))+
#   scale_fill_manual(values = "#5ab4ac") +
#   facet_wrap(~month)
# 
# #bird #c51b7d
# #birdinsect #e9a3c9
# #insect #5ab4ac
# #froginsect #4d9221
# #birdfroginsect ##9ebcda
# #temp #feb24c
# library(scales)
# 
# ggplot(insects_monthly, aes(x = TempOut, y = n)) + 
#   geom_smooth() +
#   ggsave(getDataPath("Figures", "08.03.2022_monthlyinsects_temp.jpg"))
# 
# #Hourly bird/Insects----
# birdinsects_monthly <- filter(df, RFclass == "birdinsect") %>%
#   filter(month != 202001 & month != 202002) %>% 
#   group_by(month) %>% 
#   mutate(n = n()) %>% 
#   droplevels()
# 
# ggplot(birdinsects_monthly, aes(x = HumOut, y = n)) + 
#   geom_smooth() +
#   ggsave(getDataPath("Figures", "08.03.2022_monthlybirdinsects_hum.jpg"))
# 
# 
# ggplot(birdinsects_monthly, aes(x = TempOut, y = n)) + 
#   geom_smooth() +
#   ggsave(getDataPath("Figures", "08.03.2022_monthlybirdinsects_temp.jpg"))
# 
# 
# #Hourly Bird----
# bird_monthly <- filter(df, RFclass == "bird") %>% 
#   filter(month != 202001 & month != 202002) %>% 
#   group_by(month) %>% 
#   mutate(n = n()) %>% 
#   droplevels()
# 
# ggplot(bird_monthly, aes(x = HumOut, y = n)) + 
#   geom_smooth() +
#   ggsave(getDataPath("Figures", "08.03.2022_monthlybirds_hum.jpg"))
# 
# 
# ggplot(bird_monthly, aes(x = NDVI_MEAN, y = n)) + 
#   geom_smooth() +
#   ggsave(getDataPath("Figures", "08.03.2022_monthlybirds_ndvi.jpg"))
# 
# 
# 
#   ggplot(bird_monthly, aes(x = month, y = NDVI_MEAN)) +
#   geom_smooth() +
#   ggsave(getDataPath("Figures", "08.03.2022_ndvi_month.jpg"))
# 
# df %>% filter(month != 202001 & month != 202002) %>%
#   ggplot(aes(x = month, y = n)) +
#   geom_smooth() +
#   ggsave(getDataPath("Figures", "n_month.jpg"))
# 
# df %>% 
#   ggplot(aes(x = month, y = moon_illu)) +
#   geom_smooth()
# 
plot(df$month, df$Rain)
