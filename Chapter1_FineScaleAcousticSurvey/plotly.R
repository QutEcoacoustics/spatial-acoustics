rm(list = ls())

library(tidyverse)
library(ggplot2)
library(plotly)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

data.scores <- read.csv(getDataPath(chapter, "nmds_DataScores.csv"))
species.scores <- read.csv(getDataPath(chapter, "nmds_SpeciesScores.csv"))



p_bestall <- plot_ly()
p_bestall <- add_trace(p_bestall, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p_bestall <- add_trace(p_bestall, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p_bestall <- layout(p_bestall, title = "Best model (Stress: 0.07, R2 = 0.494, p < 0.01)")
orca(p_bestall, getDataPath(chapter, "Fig1", "NMDS_ALL_OPT", "plotly_all.png"))