library(tidyverse)
library(ggplot2)
library(ggalluvial)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "SERF"

chapter <- "Chapter2_SoundscapeTemporalAssessment"


df <- read.csv(getDataPath(chapter, "DiscriminantAnalysis", paste(data, "_component_RFlabels.csv", sep = ""))) %>% 
  mutate(., subject = as.factor(1:nrow(.))) %>% 
  filter(., component != "") %>% 
  droplevels(.)
  

confusion_matrix <- table(df$component, df$component_model)

(sum(df$component==df$component_model)) / nrow(df)

graph_df <- select(df, component, component_model, subject) %>% 
  pivot_longer(., cols = 1:2,  names_to = "survey", values_to = "response")


data.frame(table(graph_df$response, graph_df$survey, graph_df$subject)) %>% 
  filter(., Freq != 0) %>% 
  ggplot(., aes(y = Freq, x = Var2, stratum = Var1, alluvium = Var3, fill = Var1, label = Var1)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum") +
  scale_fill_manual(values = c("#99d8c9", "#fdbb84", "#bdbdbd")) +
  ggsave(getDataPath(chapter, "DiscriminantAnalysis", paste(data, "_alluvial_component.png", sep = "")))
  

graph_df <- select(df, component, component_model,  classID, subject) %>% 
  pivot_longer(., cols = 1:2,  names_to = "survey", values_to = "response")


data.frame(table(graph_df$response, graph_df$survey, graph_df$subject, graph_df$classID)) %>% 
  filter(., Freq != 0) %>% 
  ggplot(., aes(y = Freq, x = Var2, stratum = Var1, alluvium = Var3, fill = Var4, label = Var1)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum") +
  scale_fill_manual(values = c("#99d8c9", "#fdbb84", "#bdbdbd", "#bcbddc", "#fa9fb5")) +
  ggsave(getDataPath(chapter, "DiscriminantAnalysis", paste(data, "_alluvial_componentandclasses.png", sep = "")))
