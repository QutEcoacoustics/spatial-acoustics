library(tidyverse)
library(purrr)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019",  ...))
  
}

files <- list.files(path = getDataPath("hobos_processed"), pattern = ".csv", full.names = T)

Hobos_df <- lapply(files, read.csv) %>%
  map(., separate, Date, into = c("month", "day", "year"), sep = "/", remove = F) %>% 
  map(., mutate, Date_mod = paste("2019", month, day, sep = "")) %>% 
  map(., separate, Time, into = c("hour", "min", "sec"), sep = ":", remove = F) %>% 
  map_df(., select, c("Date", "Time", "time_mod", "hour", "min", "sec", "temperature", "humidity", "hobo", "point", "Date_mod")) %>%
  write.csv(., "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/16.07.2020_HobosOct.csv")

df_vars <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/13.07.2020_newmodelvars.csv")
Hobos_df <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/16.07.2020_HobosOct.csv")
Hobos_df <- filter(Hobos_df, Date_mod != 20191013) %>% 
write.csv(., "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/16.07.2020_HobosOct.csv")


df_vars1 <- mutate(df_vars, minutes = case_when(minute_mod <= 9 ~ 0,
                                          10 >= minute_mod | minute_mod <= 19 ~ 10,
                                          20 >= minute_mod | minute_mod <= 29 ~ 20,
                                          30 >= minute_mod | minute_mod <= 39 ~ 30,
                                          40 >= minute_mod | minute_mod <= 49 ~ 40,
                                          50 <= minute_mod ~ 50))

write.csv(df_vars1, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/16.07.2020_teste.csv")

df_vars1 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/16.07.2020_newmodelvars.csv")

df1 <- left_join(x = df_vars1, y = Hobos_df, by = c("new_id" = "point", "Date" = "Date_mod", "hour_min_start" = "hour", "minutes" = "min"))
summary(df1)
unique(Hobos_df$time_mod)

df2 <- select(df1, X.x, ID, humidity, temperature, everything())
write.csv(df2, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/16.07.2020_newmodelvars.csv")
summary(df2[4:16])

df <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/16.07.2020_newmodelvars.csv")
df <- select(df, -humidity)
write.csv(df, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/16.07.2020_modelvars_complete.csv")

df <-  df %>% mutate_at(vars(2), scale)
summary(df[2:13])
cor(df[,2:13])

glm <- lm(AcousticComplexity ~ CanopyCover + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH + NDVI_AVERAGE + temperature, data = df)
summary(glm)
