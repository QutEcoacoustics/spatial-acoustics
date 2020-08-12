#Quick GLM to explore relationships#

library(tidyverse)
library(ggplot2)

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

#Reads the data
df <- read.csv(getDataPath("27.07.2020_IndicesMoon.csv"))

glm <- lm(Ndsi ~ moon_illu, data = df)

summary(glm)
