#GLM to test which variables are the most important shaping the indices#
#Marina Scarpelli#
#07.01.2020#

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(stringi)
library(car)
library(data.table)
library(MuMIn)

#Reading and preparing the data ####
getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"


library(vegan)
set.seed(123)
      

#putting everything together


total1 <- read.csv(getDataPath(chapter, "01.09.2021_insect_data.csv"))
  # filter(., metric == "presence") %>% 
  # select(everything(), -c(willie_wagtail, cockatiel, interference, geophony, silence, fly, hopping_kangaroo)) %>% 
  # droplevels(.)

data3 <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  select(., point, NS_DIST_AVG, SubcanopyHeight) %>% 
  distinct(.) %>% 
  droplevels(.)

merged <- merge(total1, data3, by.x = "Point", by.y = "point") %>% 
  filter(VegDescription2 != "") %>% 
  droplevels(.)




rownames(merged) <- merged$Point


df <- as.matrix(merged)
write.csv(df, getDataPath("Chapter1_FineScaleAcousticSurvey", "jaccarddist_all.csv"))

na <- is.na(total)

model <- metaMDS(comm = merged[3:5],
                 distance = "bray",
                 k = 2,
                 try = 100)

data.scores <- as.data.frame(scores(model))

data.scores$Point <- rownames(data.scores)
data.scores$veg <- merged$VegDescription2
data.scores$Point <- merged$Point

head(data.scores$veg)

write.csv(data.scores, getDataPath(chapter, "nmds_insect_data.scores.csv"))

species.scores <- as.data.frame(scores(model, "species"))
species.scores$landvar <- rownames(species.scores)

head(species.scores)

write.csv(species.scores, getDataPath(chapter, "nmds_insect_species.scores.csv"))

#Plotly - insects all land variables

library(plotly)

p <- plot_ly()
p <- add_trace(p, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p <- add_trace(p, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
#p <- layout(p, title = "All sp and land variables (Stress = 0.1)")
orca(p, getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT", "plotly_birds.png"))


PERMANOVA <- adonis(merged[3:5] ~ merged$VegDescription2)
anosim <- anosim(merged[3:5], merged$VegDescription2)

plot(dist)

#diss of sp composition per point

total1 <- read.csv(getDataPath(chapter, "sp_shrub.csv"))
  

diversity <- betadiver(total1[,3:62])
accumulation <- specaccum(total1[,3:62])
spec <- specnumber(total1[,3:62])
diss <- vegdist(total1[,3:62], method = "jaccard", binary = T)

plot(diversity)


plot(diss_values$sp_diss ~ diss_values$veg_col)
plot(total1$sp_richness ~ total1$veg_description2)

cor(x = diss_values$sp_diss, y = merged$mean, method = "spearman")
cor(x = merged$sp_richness, y = merged$sd, method = "spearman")
cor(x = merged$sp_richness, y = merged$NS_DIST_AVG, method = "spearman")
cor(x = merged$sp_richness, y = merged$SubcanopyHeight, method = "spearman")


