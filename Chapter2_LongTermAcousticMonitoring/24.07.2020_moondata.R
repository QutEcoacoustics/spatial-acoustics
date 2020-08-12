library(lunar)
?lunarlibrary(tidyverse)
library(stringr)

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("24.07.2020_MoonData.csv"))

df1 <- separate(df, col = ï..date, into = c("year", "monthday"), by = ,4, remove = F) %>% 
  separate(., col = monthday, into = c("month", "day"), by = ,2, remove = T)

df1 <- do.call(paste, c(df1[,2:4], sep="-"))
df1 <- as.data.frame(df1)



moon_illu <- lunar.illumination(as.Date(df1$df1))
data_moon <- as.data.frame(cbind(df, df1, moon_illu))
write.csv(data_moon, getDataPath("24.07.2020_MoonIllumination.csv"))

moon_data <- read.csv(getDataPath("24.07.2020_MoonIllumination.csv"))
df <- read.csv(getDataPath("SERF_AI_PreProcessed", "21.07.2020_151617FinalIndices.csv"))

complete_df <- right_join(x = moon_data, y = df, by = "date")
df_final <- select(complete_df, -c(X.x, df1, X.y))
write.csv(df_final, getDataPath("27.07.2020_IndicesMoon.csv"))
