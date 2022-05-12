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
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation", ...))
}

df <- read.csv(getDataPath("12.04.2022_completedf.csv")) %>% 
  select(everything(), -X) %>% 
  separate(date, into = c("year", "month_day"), sep = 4, remove = F) %>% 
  separate(month_day, into = c("month", "day"), sep = 2) %>% 
  mutate(date_r = paste(year, month, day, sep = "-"))

df_master <- read.csv(getDataPath("Points.csv"))

df <- left_join(df, df_master, by = c("point"))

# df <- mutate(df, lat = -27.3889) %>% 
#   mutate(lon = 152.8812)
# 
# data <- df[c(2,123:124)] %>% 
#   rename("date" = "Date")

df$date_r <- as.Date.character(df$date_r)
#df$date_time <- as.POSIXct(df$date_time)

df <- mutate(df, date_time2 = as.POSIXct(paste(date_r, Time), sep = " "))


df_test <- mutate(df, hour = case_when(ResultMinute <= 59 ~ 0,
                                       ResultMinute >= 60 ~ 10000)) %>%
  mutate(
    minutes = case_when(
      ResultMinute <= 4 ~ 0,
      5 >= ResultMinute |
        ResultMinute <= 9 ~ 500,
      10 >= ResultMinute |
        ResultMinute <= 14 ~ 1000,
      15 >= ResultMinute |
        ResultMinute <= 19 ~ 1500,
      20 >= ResultMinute |
        ResultMinute <= 24 ~ 2000,
      25 >= ResultMinute |
        ResultMinute <= 29 ~ 2500,
      30 >= ResultMinute |
        ResultMinute <= 34 ~ 3000,
      35 >= ResultMinute |
        ResultMinute <= 39 ~ 3500,
      40 >= ResultMinute |
        ResultMinute <= 44 ~ 4000,
      45 >= ResultMinute |
        ResultMinute <= 49 ~ 4500,
      50 >= ResultMinute |
        ResultMinute <= 54 ~ 5000,
      55 >= ResultMinute |
        ResultMinute <= 59 ~ 5500,
      60 >= ResultMinute |
        ResultMinute <= 64 ~ 000,
      65 >= ResultMinute |
        ResultMinute <= 69 ~ 500,
      70 >= ResultMinute |
        ResultMinute <= 74 ~ 1000,
      75 >= ResultMinute |
        ResultMinute <= 79 ~ 1500,
      80 >= ResultMinute |
        ResultMinute <= 84 ~ 2000,
      85 >= ResultMinute |
        ResultMinute <= 89 ~ 2500,
      90 >= ResultMinute |
        ResultMinute <= 94 ~ 3000,
      95 >= ResultMinute |
        ResultMinute <= 99 ~ 3500,
      100 >= ResultMinute |
        ResultMinute <= 104 ~ 4000,
      105 >= ResultMinute |
        ResultMinute <= 109 ~ 4500,
      110 >= ResultMinute |
        ResultMinute <= 114 ~ 5000,
      115 <= ResultMinute ~ 5500
    )
  ) %>%
  select(1:11, hour, minutes, time_real, everything())

df_test$time_test <- rowSums(df_test[12:14]) 

df_test <- select(df_test, 1:14, time_test, everything())

df_test2 <- separate(df_test, time_test, into = c(as.character("hourminutes"), as.character("seconds1")), sep = -2, remove = F) %>% 
  separate(., hourminutes, into = c(as.character("hour1"), as.character("minutes1")), sep = 2, remove = T) 

df_test2$new_time <- with(df_test2, paste(hour1, minutes1, 00, sep = ":"))

df_test2 <- select(df_test2, 1:18, new_time, everything())
write.csv(df_test2, getDataPath("13.04.2022_fixtime.csv"), row.names = F)

df_fixedtime <- read.csv(getDataPath("13.04.2022_fixtime.csv")) %>% 
  select(2:11, 14, 19:68) %>% 
  write.csv(getDataPath("TimeFixed.csv"), row.names = F)

df_fixedtime <- read.csv(getDataPath("TimeFixed.csv")) #Remember fixing the date format when saving


weekday <- mutate(df_fixedtime, week_day = weekdays(as.Date(df_fixedtime$date_r))) %>% 
  select(date_r, week_day, everything()) %>% 
  write.csv(., getDataPath("15.04.2022_completedf.csv"))

rm(list = ls())

df <- read.csv(getDataPath("15.04.2022_completedf.csv"))

df_sunglight <- df %>% select(date_r, lat, long) %>% 
  rename("date" = date_r,
         "lon" = long)

df <- mutate(df, date_time2 = as.POSIXct(paste(date_r, new_time), sep = " "))

df_sunglight$date <- as.Date.character(df_sunglight$date)

df$date_r <- as.Date.character(df$date_r)

sun <- getSunlightTimes(data = df_sunglight, keep = c("sunrise", "sunset", "night", "nightEnd"))

 
df2 <- rename(sun, date_r = date) %>% 
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

write.csv(df2, getDataPath("15.04.2022_completedf.csv"), row.names = F)

df <- read.csv(getDataPath("15.04.2022_completedf.csv"))

library(suncalc)

df$date_r <- as.Date.character(df$date_r)
# 
df <- mutate(df, date_time2 = as.POSIXct(paste(date_r, new_time), sep = " "))

library(lunar)

moon_illu <- mutate(df, moon_illu = lunar.illumination(date_r))

moon <- getMoonTimes(data = df_sunglight, keep = c("rise", "set"))

new_df <- rename(moon, date_r = date) %>% 
  left_join(moon_illu) %>% 
  distinct() %>%
  mutate(moon_illumination = case_when(rise >= set ~ case_when(date_time2 %within% interval(set, rise) ~ "moon_illu",
                                       TRUE ~ "no_moon"),
                                       rise <= set ~ case_when(date_time2 %within% interval(rise, set) ~ "moon_illu",
                                       TRUE ~ "no_moon"),
                                       TRUE ~ "idk")) %>% 
  select(rise, set, date_time2, moon_illumination, moon_illu, everything()) %>%
  mutate(moon_illu = case_when(moon_illumination == "no_moon" ~ 0,
                               TRUE ~ moon_illu)) %>% 
  mutate(moon_illu = case_when(period == "day" ~ 0,
                               TRUE ~ moon_illu)) %>% 
  select(rise, set, date_time2, moon_illumination, moon_illu, period, everything())
  #select(Date, moon_illu, period, week_day, TempOut, HumOut, Rain, anthrophony, geophony, EBI_RANGE, NDVI_MEAN, -moon_illumination, -date_time2, set, rise, everything())

write.csv(new_df, getDataPath("15.04.2022_completedf.csv"), row.names = F)

