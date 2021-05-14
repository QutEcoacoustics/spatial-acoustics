library(tidyverse)
library(ggplot2)
library(ggalluvial)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"

chapter <- "STSC"


df <- read.csv(getDataPath(chapter, "Test", paste(data, "_component_RFlabels.csv", sep = ""))) %>% 
  mutate(., subject = as.factor(1:nrow(.))) %>% 
  filter(., component != "" & component != "interference") %>% 
  droplevels(.)
  

confusion_matrix <- table(df$component, df$RFcomponent)

accuracy_df <- filter(df, component != "" & component != "interference") %>% 
  droplevels(.)

(sum(accuracy_df$component==accuracy_df$RFcomponent)) / nrow(accuracy_df)

graph_df <- select(df, component, RFcomponent, subject) %>% 
  pivot_longer(., cols = 1:2,  names_to = "survey", values_to = "response")


data.frame(table(graph_df$response, graph_df$survey, graph_df$subject)) %>% 
  filter(., Freq != 0) %>% 
  ggplot(., aes(y = Freq, x = Var2, stratum = Var1, alluvium = Var3, fill = Var1, label = Var1)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum") +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none", axis.line = element_blank(), plot.background = element_rect(fill = NA), panel.background = element_rect(fill = NA), axis.text.x = element_text()) +
  scale_x_discrete(labels = c("component" = "Manual labels", "RFcomponent" = "Random Forest labels")) +
  scale_fill_manual(values = c("#99d8c9", "#fdbb84", "#bdbdbd")) +
  ggsave(getDataPath(chapter, "Test", paste(data, "_alluvial_component.png", sep = "")))