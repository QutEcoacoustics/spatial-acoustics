library(tidyverse)
library(ggplot2)
library(alluvial)
library(ggalluvial)
library(easyalluvial)

set.seed(123)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

folder <- "AIndices"
step1 <- "10_RF"
round <- "Round2"

# files <- list.files(getDataPath(folder, step1, round), pattern = "_class_RFlabels.csv", full.names = T)

step2 <- "11_FinalDF"
chapter <- "chp4"

# file1 <- read.csv(files[9]) %>% 
#   select(1:23)
# 
# file2 <- read.csv(files[10]) %>% 
#   select(1:23)
#   
# final <- rbind(file1, file2)
#   
# write.csv(final, getDataPath(folder, step2, "77_chp3.csv"))

files <- list.files(getDataPath(folder, step2), pattern = "_chp4", full.names = T)

file <- files[1]

file_name <- basename(file) %>% 
  str_split(., pattern = "_chp4")

file_name <- file_name[[1]][1]


df <- read.csv(file) %>% 
  mutate(., subject = as.factor(1:nrow(.))) %>% 
  droplevels(.)

confusion_matrix <- table(df$class_category, df$general_category)

accuracy_df <- filter(df, class_category != "") %>% 
  droplevels(.)


(sum(accuracy_df$class_category==accuracy_df$general_category)) / nrow(accuracy_df)

graph_df <- select(accuracy_df, class_category, general_category, subject) %>% 
  pivot_longer(., cols = 1:2,  names_to = "survey", values_to = "response")


data.frame(table(graph_df$response, graph_df$survey, graph_df$subject)) %>% 
  filter(., Freq != 0) %>% 
  ggplot(., aes(y = Freq, x = Var2, stratum = Var1, alluvium = Var3, fill = Var1, label = Var1)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum") +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none", axis.line = element_blank(), plot.background = element_rect(fill = NA), panel.background = element_rect(fill = NA), axis.text.x = element_text()) +
  scale_x_discrete(labels = c("class_category" = "Manual labels", "general_category" = "Random Forest labels")) +
  scale_fill_manual(values = c("#e31a1c", "#fdbf6f", "#fb9a99", "#ff7f00", "#33a02c", "#b2df8a", "#1f78b4")) +
  ggsave(getDataPath(folder, chapter, "Figures", paste(file_name, "_alluvial_category.png", sep = "")))




#Colours in alphabetical order
#e31a1c
#fdbf6f
#fb9a99
#ff7f00
#33a02c
#b2df8a
#1f78b4
