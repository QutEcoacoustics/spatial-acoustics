library(tidyverse)
library(ggplot2)
library(ggalluvial)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraoct"


df <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_class_RFlabels.csv", sep = ""))) %>% 
  mutate(., subject = as.factor(1:nrow(.))) %>% 
  filter(., classID != "" & classID != "silence" & classID != "wind") %>% 
  droplevels(.)


confusion_matrix <- table(df$classID, df$class_model)

(sum(df$classID==df$class_model)) / nrow(df)

graph_df <- select(df, classID, class_model, subject) %>% 
  pivot_longer(., cols = 1:2,  names_to = "survey", values_to = "response")


data.frame(table(graph_df$response, graph_df$survey, graph_df$subject)) %>% 
  filter(., Freq != 0) %>% 
  ggplot(., aes(y = Freq, x = Var2, stratum = Var1, alluvium = Var3, fill = Var1, label = Var1)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum") +
  scale_fill_manual(values = c("#99d8c9", "#fdbb84", "#bdbdbd"))


graph_df <- select(df, classID, class_model,  component, subject) %>% 
  pivot_longer(., cols = 1:2,  names_to = "survey", values_to = "response")


data.frame(table(graph_df$response, graph_df$survey, graph_df$subject)) %>% 
  filter(., Freq != 0) %>% 
  ggplot(., aes(y = Freq, x = Var2, stratum = Var1, alluvium = Var3, fill = Var1, label = Var1)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum") +
  scale_fill_manual(values = c("#99d8c9", "#fdbb84", "#bdbdbd", "#bcbddc", "#fa9fb5"))
