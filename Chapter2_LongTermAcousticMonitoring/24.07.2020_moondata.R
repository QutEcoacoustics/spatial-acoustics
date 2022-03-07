library(lunar)
library(tidyverse)
library(stringr)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/", ...))
}

df <- read.csv(getDataPath("AIndices", "11_FinalDF", "64_253_chp4_class_RFlabels.csv"))

df1 <- separate(df, col = date, into = c("year", "monthday"), by = ,4, remove = F) %>% 
  separate(., col = monthday, into = c("month", "day"), by = ,2, remove = T) %>% 
  select(year, month, day)

df1 <- do.call(paste, c(df1[,1:3], sep="-"))
df1 <- as.data.frame(df1)



moon_illu <- lunar.illumination(as.Date(df1$df1))
data_moon <- as.data.frame(cbind(df, df1, moon_illu)) %>% 
  select(1:25, df1, moon_illu, everything()) %>% 
  rename("date_r_format" = "df1")
write.csv(data_moon, getDataPath("Chapter2_SoundscapeTemporalAssessment", "05.02.2022_completedf.csv"), row.names = F)
