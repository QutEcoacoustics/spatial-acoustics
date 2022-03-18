#17.03.2022 More plots

library(tidyverse)
library(ggplot2)
library(markdown)
library(randomForest)
library(scales)
library(lubridate)



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

plot_df$Recording_time <- as.character(plot_df$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00")) 
  
plot_df <- mutate(plot_df, Recording_time = case_when(Recording_time == "0:00:00" ~ "24:00:00",
                                                                            TRUE ~ Recording_time))

plot_df$date_time3 <- as.POSIXct(paste(plot_df$Date, plot_df$Recording_time, sep = " "))

#Monthly----
plot_monthly <- select(plot_df, month, RFclass, NDVI_MEAN, HumOut, TempOut) %>% 
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

#Bird
plot_monthly %>% filter(RFclass == "bird" & variables != "Temp") %>%
  droplevels() %>% 
  #mutate(scaled = scale(mean)) %>% 
ggplot(., aes(x=month, y = scaled, colour = variables)) +
  geom_smooth() +
  scale_colour_manual(values = c("#c51b7d", "#0570b0", "#238b45"), labels = c("Bird", "Humidity", "NDVI")) +
  labs(colour = "Variables", x = "Month", y = "Scaled values") +
  theme_light() +
  ggsave(getDataPath("Figures", "GoodFigs", "18.03.2022_birdmonthvars.jpg"))
  

#birds "#c51b7d"
#Hum "#0570b0"
#NDVI "#238b45"
#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac


#Insects
plot_monthly %>% filter(RFclass == "insect" & variables != "Humidity") %>% 
  droplevels() %>% 
  #mutate(scaled = scale(mean)) %>% 
  ggplot(., aes(x=month, y = scaled, colour = variables)) +
  geom_smooth() +
  scale_colour_manual(values = c("#5ab4ac", "#238b45", "#fd8d3c"), labels = c("Insect", "NDVI", "Temperature")) +
  labs(colour = "Variables", x = "Month", y = "Scaled values") +
  theme_light() +
  ggsave(getDataPath("Figures", "GoodFigs", "18.03.2022_insectmonthvars.jpg"))

#InsectBird

plot_monthly_insectbird <- select(plot_df, month, RFclass, HumOut, TempOut) %>% 
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

plot_monthly_insectbird %>% filter(RFclass == "birdinsect") %>% 
  droplevels() %>% 
  ggplot(., aes(x=month, y = scaled, colour = variables)) +
  geom_smooth() +
  scale_colour_manual(values = c("#e9a3c9", "#0570b0", "#fd8d3c"), labels = c("Cicadas", "Humidity", "Temperature")) +
  labs(colour = "Variables", x = "Month", y = "Scaled values") +
  theme_light() +
  ggsave(getDataPath("Figures", "GoodFigs", "18.03.2022_birdinsectmonthvars.jpg"))
  
  #birds "#c51b7d"
  #Hum "#0570b0"
  #NDVI "#238b45"
  #bird #c51b7d
  #birdinsect #e9a3c9
  #insect #5ab4ac
 #Temp "#fd8d3c"


#Daily ----



plot_monthly <- select(plot_df, month, RFclass, moon_illu, HumOut, TempOut, date_time3) %>% 
  na.exclude(.) %>% 
  group_by(., month, date_time3, RFclass) %>% 
  summarise(count_class = n(),
            moon = mean(moon_illu),
            Humidity = mean(HumOut),
            Temp = mean(TempOut)) %>% 
  pivot_longer(., cols = c(moon, count_class, Humidity, Temp), values_to = "mean", names_to = "variables") %>% 
  mutate(scaled = scale(mean)) %>% 
  mutate(., month = case_when(month == as.character("202001") ~ as.character("January"),
    month == as.character("202002") ~ as.character("February"),
    month == as.character("202003") ~ as.character("March"),
    month == as.character("202004") ~ as.character("April"),
    month == as.character("202005") ~ as.character("May"),
    month == as.character("202006") ~ as.character("June"),
    month == as.character("202007") ~ as.character("July"),
    month == as.character("202008") ~ as.character("August"),
    month == as.character("202009") ~ as.character("September"),
    month == as.character("202010") ~ as.character("October"),
    month == as.character("202011") ~ as.character("November"),
    month == as.character("202012") ~ as.character("December"), 
    TRUE ~ as.character(month)))

#labels = c("0:00:00" = "0:00", "2:00:00" = "2:00", "4:00:00" = "4:00", "6:00:00" = "6:00", "8:00:00" = "8:00", "10:00:00" = "10:00", "12:00:00" = "12:00", "14:00:00" = "14:00", "16:00:00" = "16:00", "18:00:00" = "18:00", "20:00:00" = "20:00", "22:00:00" = "22:00"

plot_monthly %>% filter(RFclass == "bird" & variables != "moon") %>% 
  droplevels() %>%
  na.exclude() %>% 
ggplot(., aes(x = date_time3, y = scaled, colour = variables)) + 
  geom_smooth() +
  #scale_fill_manual(values = c("#c51b7d", "#9ebcda", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  #labs(fill = "Sound class", x = "Time", y = "Sound class count") +
  scale_x_datetime() +
  theme_light() +
  facet_wrap(.~month)
  ggsave(getDataPath("Figures", "GoodFigs", "15.03.2022_RosePlot_hourly.jpg"), width = 16, height = 12)
