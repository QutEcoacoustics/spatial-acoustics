rm(list = ls())
library(tidyverse)
library(ggplot2)
library(rmarkdown)
library(zoo)
library(forecast)
library(report)
library(suncalc)
library(lubridate)
library(TTR)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment", ...))
}

df <- read.csv(getDataPath("15.02.2022_completedf.csv"))

# df <- mutate(df, lat = -27.3889) %>% 
#   mutate(lon = 152.8812)
# 
# data <- df[c(2,123:124)] %>% 
#   rename("date" = "Date")

df$Date <- as.Date.character(df$Date)
df$date_time <- as.Date(df$date_time)

df <- mutate(df, date_time2 = as.POSIXct(paste(Date, Time), sep = " "))

sun <- getSunlightTimes(date = df$Date, lat = -27.3889, lon = 152.8812, tz = "Australia/Brisbane", keep = c("sunrise", "sunset", "night", "nightEnd"))

 
df2 <- rename(sun, Date = date) %>% 
  left_join(df) %>% 
  distinct() %>%
  mutate(period = case_when(date_time2 %within% interval(sunset, night) ~ 'dusk',
                            date_time2 %within% interval(nightEnd, sunrise) ~ 'dawn',
                            date_time2 %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ 'night'))


#the code should be that below but for some reason this didn't work so I made a tweak that worked - as per above
# mutate(period = case_when(date_time2 %within% interval(sunset, night) ~ 'dusk',
#                           date_time2 %within% interval(nightEnd, sunrise) ~ 'dawn',
#                           date_time2 %within% interval(sunrise, sunset) ~ 'day',
#                           date_time2 %within% interval(night, nightEnd) ~ 'night'))

write.csv(df2, getDataPath("22.02.2022_completedf.csv"), row.names = F)

df <- read.csv(getDataPath("22.02.2022_completedf.csv"))

library(suncalc)

df$Date <- as.Date.character(df$Date)

df <- mutate(df, date_time2 = as.POSIXct(paste(Date, Time), sep = " "))

moon <- getMoonTimes(date = df$Date, lat = -27.3889, lon = 152.8812, tz = "Australia/Brisbane", keep = c("rise", "set"))

#moon <- separate(sun, into = c("moon_rise_date", "moon_rise_time"), col = "rise", sep = " ") %>% 
  #separate(into = c("moon_set_date", "moon_set_time"), col = "set", sep = " ")

rm(new_df)

df <- mutate(df, date_time2 = as.POSIXct(paste(Date, Time), sep = " "))

new_df <- rename(moon, Date = date) %>% 
  left_join(df) %>% 
  distinct() %>%
  mutate(moon_illumination = case_when(date_time2 %within% interval(rise, set) ~ "moon_illu",
                                       TRUE ~ "no_moon")) %>% 
  select(rise, set, date_time2, moon_illumination, everything()) %>% 
  mutate(moon_illu = case_when(moon_illumination == "no_moon" ~ 0,
                               TRUE ~ moon_illu)) %>% 
  select(Date, moon_illu, period, week_day, TempOut, HumOut, Rain, anthrophony, geophony, EBI_RANGE, NDVI_MEAN, -moon_illumination, -date_time2, set, rise, everything())

write.csv(new_df, getDataPath("24.02.2022_completedf.csv"), row.names = F)

