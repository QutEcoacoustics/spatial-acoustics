rm(list = ls())

library(tidyverse)
library(ggplot2)
library(plotly)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

#nmds vegetation


# Call:
#   adonis(formula = df_newveg1[, c(78, 97)] ~ df_newveg1$VegDescription2) 
# 
# Permutation: free
# Number of permutations: 999
# 
# Terms added sequentially (first to last)
# 
# Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# df_newveg1$VegDescription2    4    63.348 15.8371  437.16 0.49416  0.001 ***
#   Residuals                  1790    64.847  0.0362         0.50584           
# Total                      1794   128.195                 1.00000           
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


data.scores <- read.csv(getDataPath(chapter, "nmds_DataScores.csv"))
species.scores <- read.csv(getDataPath(chapter, "nmds_SpeciesScores.csv"))

colours <- c( "#35978f", "#01665e", "#80cdc1", "#8c510a", "#bf812d")



p_bestall <- plot_ly()
p_bestall <- add_trace(p_bestall, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point, colors = colours, color = data.scores$veg)
p_bestall <- add_trace(p_bestall, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
#p_bestall <- layout(p_bestall, title = "Best model (Stress: 0.07, R2 = 0.494, p < 0.01)")
orca(p_bestall, getDataPath(chapter, "Fig1", "NMDS_ALL_OPT", "plotly_all.png"))



#nmds species

data.scores <- read.csv(getDataPath(chapter, "nmds_sp_data.scores.csv"))
species.scores <- read.csv(getDataPath(chapter, "nmds_sp_species.scores.csv"))

#mutate(as.character(species.scores$landvar), "NS_DIST_AVG" == "Distance to nearest shrub")

# Call:
#   adonis(formula = total ~ merged$veg_description2) 
# 
# Permutation: free
# Number of permutations: 999
# 
# Terms added sequentially (first to last)
# 
# Df SumsOfSqs MeanSqs F.Model     R2 Pr(>F)    
# merged$veg_description2   3     3.657 1.21900  12.749 0.2372  0.001 ***
#   Residuals               123    11.761 0.09561         0.7628           
# Total                   126    15.418                 1.0000           
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

colours <- c( "#35978f", "#01665e", "#80cdc1", "#8c510a", "#bf812d")


p <- plot_ly()
p <- add_trace(p, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point, colors = colours, color = data.scores$veg)
p <- add_trace(p, name = "Species", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p <- layout(p, title = "All sp and land variables (Stress = 0.25, R2 = 0.23, p<0.01)")
orca(p, getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT", "plotly_birds.png"))