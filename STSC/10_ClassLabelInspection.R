library(tidyverse)
library(ggplot2)
library(ggalluvial)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"

chapter <- "STSC"


df <- read.csv(getDataPath(chapter, "Test", paste(data, "_class_RFlabels.csv", sep = ""))) %>% 
  mutate(., subject = as.factor(1:nrow(.))) %>% 
  droplevels(.)

confusion_matrix <- table(df$class, df$RFclass)

accuracy_df <- filter(df, class == "insect" | class == "bird") %>% 
  droplevels(.)

(sum(accuracy_df$class==accuracy_df$RFclass)) / nrow(accuracy_df)


graph_df <- select(accuracy_df, class, RFclass, subject) %>% 
  pivot_longer(., cols = 1:2,  names_to = "survey", values_to = "response")


data.frame(table(graph_df$response, graph_df$survey, graph_df$subject)) %>% 
  filter(., Freq != 0) %>% 
  mutate(Var2 = fct_relevel(Var2, "class", "RFclass")) %>% 
  ggplot(., aes(y = Freq, x = Var2, stratum = Var1, alluvium = Var3, fill = Var1, label = Var1)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum") +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none", axis.line = element_blank(), plot.background = element_rect(fill = NA), panel.background = element_rect(fill = NA), axis.text.x = element_text()) +
  scale_x_discrete(labels = c("RFclass" = "Random Forest labels", "class" = "Manual labels")) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4")) +
  ggsave(getDataPath(chapter, "Test", paste(data, "_alluvial_class.png", sep = "")))
