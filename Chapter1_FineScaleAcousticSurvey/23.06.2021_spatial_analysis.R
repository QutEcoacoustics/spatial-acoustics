#Spatial model

rm(list = ls())

library(tidyverse)


#Reading and preparing the data ####
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

data <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  mutate(., period = case_when(hour > 4 & hour <= 8 ~ "dawn",
                                              hour > 8 & hour <= 12 ~ "morning",
                                              hour > 12 & hour <= 16 ~ "afternoon",
                                              hour > 16 & hour <= 20 ~ "dusk",
                                              hour > 20 & hour <= 24 ~ "evening",
                                              T ~ "night")) %>% 
  # select(everything(), -index_value) %>% 
  group_by(point, index, period, class_model) 


land_data <- data %>% 
  select(21:101)

complete_df <- data %>% 
  summarise(mean(mean), mean(sd))

final_spatial <-left_join(complete_df, land_data) %>% 
  write.csv(getDataPath(chapter, "22.06.2021_indicesavgcomplete.csv"))
