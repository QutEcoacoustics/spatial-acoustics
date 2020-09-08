library(ggplot2)
library(tidyverse)

df <- read.csv("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/Masters_Project/Cap2/1_DataPoints_Splitted/06.09.2020_GraphDF.csv") %>% 
  pivot_longer(., cols = 3:8, names_to = "class", values_to = "percentage")

facet_labels <- c("100m buffer", "2km buffer")

ggplot(df, aes(x = Environment, y = percentage, fill = class)) +
  geom_col() +
  theme_classic() +
  scale_fill_manual(values = c("#c4ba00", "#6ee1af", "#108614", "#785505", "#828282", "#0000ff"), name = "Classes", labels = c("Crop Yield", "Eucaliptus Yield", "Natural Vegetation", "Pasture", "Urban", "Water")) +
  ylab("Percentage") +
  facet_wrap(. ~ Buffer) +
  ggsave("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/Masters_Project/Cap2/Images_Paper/06.09.2020_EnvironmentClasses_Buffers.png")

       