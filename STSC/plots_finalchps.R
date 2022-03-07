library(tidyverse)
library(ggplot2)


set.seed(123)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

folder <- "AIndices"

step1 <- "11_FinalDF"
chapter <- "chp4"

files <- list.files(getDataPath(folder, step1), pattern = "_chp4", full.names = T)

file <- files[1]

file_name <- basename(file) %>% 
  str_split(., pattern = "_chp3")

file_name <- file_name[[1]][1]

df <- read.csv(file) %>% 
  #filter(RFclass != "geo") %>% 
  droplevels(.)


plot_df <- group_by(df, RFclass, general_category) %>% 
  #filter(., RFclass != "geo") %>% 
  summarise(total = n()) %>% 
  mutate(., percentage = ((total/sum(plot_df$total))*100)) %>% 
  droplevels(.)

ggplot(plot_df, aes(x = RFclass, fill = general_category, y = percentage)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#e31a1c", "#fdbf6f", "#fb9a99", "#ff7f00", "#33a02c", "#b2df8a", "#1f78b4")) +
  ggsave(height = 7.46, width = 12, getDataPath(folder, chapter, "Figures", paste(file_name, "_barplot_allcategories.png", sep = "")))

#Colours in alphabetical order
#e31a1c anthrophony
#fdbf6f anthrophony/biophony
#fb9a99 anthrophony/biophony/geophony 
#ff7f00 anthrophony/geophony
#33a02c biophony
#b2df8a biophony/geophony
#1f78b4 geophony

