rm(list = ls())
library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment", ...))
}

weather <- read.csv(getDataPath("05.02.2022_WeatherData.csv")) %>% 
  select(Date, Time, TempOut, HumOut, Rain) #removing wind speed and rain rate because they are correlated with humidity and rain respectively

df <- read.csv(getDataPath("05.02.2022_completedf.csv")) %>% 
  select(everything(), -X)

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

df_test1$time_test <- rowSums(df_test[12:14]) 
 
df_test1 <- select(df_test1, 1:14, time_test, everything())

df_test2 <- separate(df_test1, time_test, into = c(as.character("hourminutes"), as.character("seconds1")), sep = -2, remove = F) %>% 
  separate(., hourminutes, into = c(as.character("hour1"), as.character("minutes1")), sep = 2, remove = T) 
  
df_test2$new_time <- with(df_test2, paste(hour1, minutes1, 00, sep = ":"))
  
df_test2 <- select(df_test2, 1:18, new_time, everything())
write.csv(df_test2, getDataPath("Weather_fixtime.csv"), row.names = F)

df_fixedtime <- read.csv(getDataPath("Weather_fixtime.csv")) %>% 
  select(2:12, 15, 20:119) %>% 
  write.csv(getDataPath("TimeFixed.csv"), row.names = F)

df_fixedtime <- read.csv(getDataPath("TimeFixed.csv")) #Remember fixing the date format when saving

merged_df <- merge(weather, df_fixedtime, by.x = c("Date", "Time"), by.y = c("date_r_format", "new_time"), all.y = T)
write.csv(merged_df, getDataPath("07.02.2022_completedf.csv"), row.names = F)

data <- read.csv(getDataPath("07.02.2022_completedf.csv"))
weekday <- mutate(data, week_day = weekdays(as.Date(data$Date))) %>% 
  select(Date, week_day, everything()) %>% 
  write.csv(., getDataPath("08.02.2022_completedf.csv"))
