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
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"


library(vegan)
set.seed(123)
      

#putting everything together


total1 <- read.csv(getDataPath(chapter, "21.10.2021_motifs_birdID.csv")) %>% 
  filter(., metric == "presence") %>% 
  select(everything(), -c(willie_wagtail, cockatiel, interference, geophony, silence, fly, hopping_kangaroo, bat_austronomus, alarm_calls, rasp_calls, dove, small_thornbill, parrot, triller)) %>% 
  droplevels(.)

data3 <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  select(., id, point, NS_DIST_AVG, SubcanopyHeight) %>% 
  distinct(.) %>% 
  droplevels(.)

merged <- merge(total1, data3, by.x = "motif_id", by.y = "id") %>% 
  droplevels(.)




rownames(merged) <- merged$motif_id

total <- merged[,c(7:68,71,72)] %>% 
  droplevels(.)


summary(total)


df <- as.matrix(dist)
write.csv(df, getDataPath("Chapter1_FineScaleAcousticSurvey", "jaccarddist_all.csv"))

na <- is.na(total)

model <- metaMDS(comm = total,
                 distance = "jaccard",
                 k = 2,
                 try = 200)

data.scores <- as.data.frame(scores(model))

data.scores$motif_id <- rownames(data.scores)
data.scores$veg <- merged$veg_description2
data.scores$point <- merged$point1

head(data.scores$veg)

write.csv(data.scores, getDataPath(chapter, "24.10.2021_nmds_sp_data.scores.csv"))

species.scores <- as.data.frame(scores(model, "species"))
species.scores$landvar <- rownames(species.scores)

head(species.scores)

write.csv(species.scores, getDataPath(chapter, "24.10.2021_nmds_sp_species.scores.csv"))

#Plotly - insects all land variables

library(plotly)

p <- plot_ly()
p <- add_trace(p, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p <- add_trace(p, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p <- layout(p, title = "All sp and land variables (Stress = 0.25)")
orca(p, getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT", "plotly_birds.png"))


PERMANOVA <- adonis(total ~ merged$veg_description2)
anosim <- anosim(total, merged$veg_description2)

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

#NMDS birds and feeding habits ----
total <- read.csv(getDataPath(chapter, "21.10.2021_motifsID_vegdesc.csv"))

rownames(total) <- total$ï..point



model <- metaMDS(comm = total[,3:67],
                 distance = "jaccard",
                 k = 2,
                 try = 200)

data.scores <- as.data.frame(scores(model))

data.scores$motif_id <- rownames(data.scores)
data.scores$veg <- total$veg_description
data.scores$point <- total$yellow_throated_miner

head(data.scores$veg)

write.csv(data.scores, getDataPath(chapter, "24.10.2021_nmds_sp_data.scores1.csv"))

species.scores <- as.data.frame(scores(model, "species"))
species.scores$landvar <- rownames(species.scores)

head(species.scores)

write.csv(species.scores, getDataPath(chapter, "24.10.2021_nmds_sp_species.scores1.csv"))

#Plotly - insects all land variables

library(plotly)

p <- plot_ly()
p <- add_trace(p, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p <- add_trace(p, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p <- layout(p, title = "All sp and land variables (Stress = 0.25)")
orca(p, getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT", "plotly_birds.png"))


PERMANOVA <- adonis(total[,3:67] ~ total$veg_description)
anosim <- anosim(total, merged$veg_description2)
