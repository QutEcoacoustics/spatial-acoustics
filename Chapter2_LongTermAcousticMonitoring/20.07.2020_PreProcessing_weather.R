library(tidyverse)
library(ggplot2)
library(purrr)

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

directory <- (getDataPath("WeatherData", "Fluxtower"))

output_dir <- (getDataPath())

files <- list.files(directory, pattern = ".csv", full.names = T, recursive = T)

files <- as.list(files)
df <- lapply(files, read.csv) 
df<- do.call(rbind, df)

df1 <- separate(df, col = TIMESTAMP, into = c("date", "time"), sep = " ", remove = F) %>% 
  separate(., col = date, into = c("day", "month", "year"), sep = "/", remove = T) %>% 
  separate(., col = time, into = c("hour", "minute"), sep = ":", remove = T) %>%
  mutate(., "date" = paste(year, month, day, sep = "")) %>% 
  mutate(., "seconds" = "00") %>% 
  mutate(., "time" = paste(hour, minute, seconds, sep = "")) %>% 
  select(., -c("day", "month", "year")) %>% 
  write.csv(., getDataPath("29.07.2020_FluxtowerComplete_Processed.csv"))

weather_data <- read.csv(getDataPath("29.07.2020_FluxtowerComplete_Processed.csv"))
indices_data <- read.csv(getDataPath("27.07.2020_IndicesMoon.csv")) %>% 
  separate(., col = time, into = c("hour", "minutes", "second"), sep = 2)

df_vars1 <- mutate(df_vars, minutes = case_when(minute_mod <= 9 ~ 0,
                                                10 >= minute_mod | minute_mod <= 19 ~ 10,
                                                20 >= minute_mod | minute_mod <= 29 ~ 20,
                                                30 >= minute_mod | minute_mod <= 39 ~ 30,
                                                40 >= minute_mod | minute_mod <= 49 ~ 40,
                                                50 <= minute_mod ~ 50))


df_complete <- right_join(weather_data, indices_data, by = c("date", "hour", "minute"))