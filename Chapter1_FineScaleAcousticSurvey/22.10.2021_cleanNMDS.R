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
library(vegan)

#Reading and preparing the data ####
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

complete_df <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  mutate_at(c(73:80, 87:101), scale)

#Removing highly correlated variables ### removed: SAVI, mean humidity, nt height, ns height and litter####
# cor <- cor(bird_df[c(73:80, 87:101)]) %>% 
#   write.csv(getDataPath(chapter, "27.02.2021_birdcorrelation_landvariables.csv"))

#NMDS ----

library(vegan)
set.seed(123)

nmds_df <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv"))

land_var <- read.csv(getDataPath("Fieldwork_Bowra", "26.02.2021_dataSAVINDVI.csv"))

land_var$aug_ndvi_avg <- as.numeric(land_var$aug_ndvi_avg)
 
land_var <- filter(land_var, NewVegDescription != "") %>% 
  mutate_at(., vars(NT_N_DIST, NT_W_DIST, NT_S_DIST, NT_E_DIST, NS_N_DIST, NS_W_DIST, NS_S_DIST, NS_E_DIST), ~ replace(., is.na(.), 100)) %>%
    mutate_at(., vars(NT_N_HEIGHT, NT_S_HEIGHT, NT_W_HEIGHT, NT_E_HEIGHT, NS_N_HEIGHT, NS_S_HEIGHT, NS_E_HEIGHT, NS_W_HEIGHT), ~replace(., is.na(.), 0)) %>%
    mutate_at(., vars(GC_NF_W, Slope, Aspect, Elevation, DistWater, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, aug_ndvi_avg, aug_savi_avg), ~replace(., is.na(.), 0)) %>%
    mutate(NT_DIST_AVG = (NT_N_DIST + NT_S_DIST + NT_E_DIST + NT_W_DIST)/4) %>%
    mutate(NT_HEIGHT_AVG = (NT_N_HEIGHT + NT_S_HEIGHT + NT_E_HEIGHT + NT_W_HEIGHT)/4) %>%
    mutate(NS_DIST_AVG = (NS_N_DIST + NS_S_DIST + NS_E_DIST + NS_W_DIST)/4) %>%
    mutate(NS_HEIGHT_AVG = (NS_N_HEIGHT + NS_S_HEIGHT + NS_E_HEIGHT + NS_W_HEIGHT)/4) %>%
    mutate(GC_NG_AVG = (GC_NG_N + GC_NG_S + GC_NG_E + GC_NG_W)/4) %>%
    mutate(GC_NF_AVG = (GC_NF_N + GC_NF_S + GC_NF_E + GC_NF_W)/4) %>%
    mutate(GC_BS_AVG = (GC_BS_N + GC_BS_S + GC_BS_E + GC_BS_W)/4) %>%
    mutate(GC_LT_AVG = (GC_LT_N + GC_LT_S + GC_LT_E + GC_LT_W)/4) %>%
    mutate(GC_SH_AVG = (GC_SH_N + GC_SH_S + GC_SH_E + GC_SH_W)/4) %>% 
  select(., NT_DIST_AVG, NT_HEIGHT_AVG, NS_DIST_AVG, NS_HEIGHT_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG, aug_ndvi_avg, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, Point, NewVegDescription, VegDescription2) %>%
  droplevels(.)

write.csv(land_var, getDataPath("8.11.2021_test.csv"))

df_newveg1 <- select(land_var, Point, NewVegDescription, VegDescription2) %>% 
  merge(., nmds_df, by.x = "Point", by.y = "point") %>% 
  mutate_at(c(75:82, 89, 91, 95, 97, 99:103), decostand, method = "range")
  

rownames(df_newveg1) <- df_newveg1$Point 

#For loop when I was iterating lots of variables to test best stress etc ----
# for (i in 1:nrow(all)){
#   
#   outcome <- NULL 
#   model <- NULL
#   PERMANOVA <- NULL
#   perm <- NULL
#   
#   outcome <- as.character(all[i,]) %>% 
#     na.exclude(.) %>% 
#     paste(., sep = ",")
#   
#   skip_to_next <- FALSE
#   
#   tryCatch({
#     model <- metaMDS(df_newveg1[outcome],
#                      dist = "bray",
#                      k = 2,
#                      try = 100)
#     
#     
#     PERMANOVA <- adonis(df_newveg1[outcome]~df_newveg1$VegDescription2)
    
    
    
    # png(filename=paste("C:/test/insects", rownames(all[1,]), "veg_TAKE4", ".png", sep = ""))
    # 
    # plot(model)
    # ordiplot(model,type="n")
    # orditorp(model,display="sites",cex=0.9,air=0.01)
    # points(model, col= colours[df_insects$VegDescription2], pch = 16)
    # orditorp(model, display="species",col="red",air=0.5)
    # ordihull(model, groups= df_insects$VegDescription2, lty = line_type)
    # legend("topleft", legend = unique(df_insects$VegDescription2), fill = colours, cex = 0.6)
    # legend("bottomleft", legend = unique(df_insects$VegDescription2), lty = line_type, cex = 0.6)
    # 
    # dev.off()
    
    
    #This one only for the complete models - with all groups
    
    # perm<-adonis(df_newveg[outcome]~df_newveg$class_model)
    # 
    # 
    # 
    # png(filename=getDataPath("Chapter1_FineScaleAcousticSurvey", "Fig1", "NMDS_birds", paste(rownames(all[i,]), "class", ".png", sep = "")))
    # 
    # plot(model)
    # ordiplot(model,type="n")
    # orditorp(model,display="sites",cex=0.9,air=0.01, labels = F)
    # points(model, col= colours2[df_newveg$class_model], pch=16)
    # ordihull(model, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
    # legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
    # legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)
    # dev.off()
    
    
    
    
#     scores_temp$model_var <- as.character(rownames(all[i,]))
#     scores_temp$conv <- as.character(model$converged)
#     scores_temp$stress <- as.numeric(model$stress)
#     scores_temp$permanova_veg_F <- as.numeric(PERMANOVA$aov.tab$F.Model[1])
#     scores_temp$permanova_veg_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
#     scores_temp$permanova_veg_p <- as.numeric(PERMANOVA$aov.tab$Pr[1])
#     # scores_temp$permanova_class_F <- as.numeric(perm$aov.tab$F.Model[1])
#     # scores_temp$permanova_class_R2 <- as.numeric(perm$aov.tab$R2[1])
#     # scores_temp$permanova_class_p <- as.numeric(perm$aov.tab$Pr[1])
#     
#     write.csv(scores_temp, getDataPath("Chapter1_FineScaleAcousticSurvey", "Fig1", "NMDS_ALL_OPT", paste(rownames(all[i,]), "veg_TAKE4", ".csv", sep = ""))) },
#     
#     #scores <- rbind(scores, scores_temp) },
#     
#     error = function(e) {skip_to_next <<-TRUE })
#   
#   if(skip_to_next) { next }
# }
  

# files <- list.files(getDataPath(chapter, "Fig1", "NMDS_ALL_OPT"), pattern = "veg_TAKE4.csv", full.names = T)
# 
# results <- lapply(files, read.csv) %>% 
#   map(., select, model_var, conv, stress, permanova_veg_F, permanova_veg_R2, permanova_veg_p) %>% 
#   do.call(rbind, .) %>% 
#   filter(., conv == "TRUE") %>% 
#   filter(., stress < 0.1) %>% 
#   write.csv(getDataPath(chapter, "Fig1", "NMDS_ALL_OPT", "filtered_nmds_all4.csv"), row.names = F)

#Investigating the points and why they are organised in this way and improving plots
#Insects:

library(plotly)


#all motifs:

#Best model - files are saved so don't need to run again----
set.seed(123)

# df_all <- select(df_newveg1, SubcanopyHeight, NS_DIST_AVG)
# 
# best_all <- metaMDS(df_all,
#                       dist = "bray",
#                       k = 2,
#                       try = 100)
# 
# 
# 
# data.scores <- as.data.frame(scores(best_all))
# 
# data.scores$id <- rownames(best_all)
# data.scores$veg <- df_newveg1$VegDescription2
# data.scores$point <- df_newveg1$Point
# 
# head(data.scores$veg)
# 
# species.scores <- as.data.frame(scores(best_all, "species"))
# species.scores$landvar <- rownames(species.scores)
# 
# head(species.scores)

# write.csv(data.scores, getDataPath(chapter, "nmds_DataScores.csv"))
# write.csv(species.scores, getDataPath(chapter, "nmds_SpeciesScores.csv"))

data.scores <- read.csv(getDataPath(chapter, "nmds_DataScores.csv"))
species.scores <- read.csv(getDataPath(chapter, "nmds_SpeciesScores.csv"))

p_bestall <- plot_ly()
p_bestall <- add_trace(p_bestall, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p_bestall <- add_trace(p_bestall, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p_bestall <- layout(p_bestall, title = "Best model (Stress: 0.07, R2 = 0.494, p < 0.01)")
orca(p_bestall, getDataPath(chapter, "Fig1", "NMDS_ALL_OPT", "plotly_all.png"))

PERMANOVA <- adonis(df_newveg1[,c(78,97)]~df_newveg1$VegDescription2)

plot(df_newveg1$VegDescription2, df_newveg1$SubcanopyHeight)

#Birds:

#Best model

df_birds <- filter(df_newveg1, class_model == "bird") %>% 
  select(., SubcanopyHeight, NS_DIST_AVG)

best_birds <- metaMDS(df_birds,
                      dist = "bray",
                      k = 2,
                      try = 100)

df_birds <- filter(df_newveg1, class_model == "bird")

data.scores <- as.data.frame(scores(best_birds))

data.scores$id <- rownames(best_birds)
data.scores$veg <- df_birds$VegDescription2
data.scores$point <- df_birds$Point

head(data.scores$veg)

species.scores <- as.data.frame(scores(best_birds, "species"))
species.scores$landvar <- rownames(species.scores)

head(species.scores)

p_bestbirds <- plot_ly()
p_bestbirds <- add_trace(p_bestbirds, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p_bestbirds <- add_trace(p_bestbirds, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p_bestbirds <- layout(p_bestbirds, title = "Birds - best model (Stress: 0.07)")

#Plotly - birds all land variables

df_birds <- filter(df_newveg1, class_model == "bird") %>% 
  select(., SubcanopyHeight, DistWater, aug_ndvi_avg , NS_DIST_AVG, GC_NF_AVG, GC_BS_AVG)

complete_birds <- metaMDS(df_birds,
                          dist = "bray",
                          k = 2,
                          try = 100)

df_birds <- filter(df_newveg1, class_model == "bird")

data.scores <- as.data.frame(scores(complete_birds))

data.scores$id <- rownames(complete_birds)
data.scores$veg <- df_birds$VegDescription2
data.scores$point <- df_birds$Point

head(data.scores$veg)

species.scores <- as.data.frame(scores(complete_birds, "species"))
species.scores$landvar <- rownames(species.scores)

head(species.scores)

p_completebirds <- plot_ly()
p_completebirds <- add_trace(p_completebirds, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p_completebirds <- add_trace(p_completebirds, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p_completebirds <- layout(p_completebirds, title = "Birds - all landscape variables (Stress:0.19)")


#Insects:

#Best model

df_insects <- filter(df_newveg1, class_model == "insect") %>% 
  select(., SubcanopyHeight, NS_DIST_AVG)

best_insects <- metaMDS(df_insects,
                      dist = "bray",
                      k = 2,
                      try = 100)

df_insects <- filter(df_newveg1, class_model == "bird")

data.scores <- as.data.frame(scores(best_insects))

data.scores$id <- rownames(best_insects)
data.scores$veg <- df_insects$VegDescription2
data.scores$point <- df_insects$Point

head(data.scores$veg)

species.scores <- as.data.frame(scores(best_insects, "species"))
species.scores$landvar <- rownames(species.scores)

head(species.scores)

p_bestinsects <- plot_ly()
p_bestinsects <- add_trace(p_bestinsects, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p_bestinsects <- add_trace(p_bestinsects, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p_bestinsects <- layout(p_bestinsects, title = "Insects - best model (Stress: 0.07)")

#Plotly - birds all land variables

df_insects <- filter(df_newveg1, class_model == "insect") %>% 
  select(., SubcanopyHeight, GC_BS_AVG, DistWater, GC_NF_AVG, NS_DIST_AVG, GC_SH_AVG)

complete_insects <- metaMDS(df_insects,
                          dist = "bray",
                          k = 2,
                          try = 100)

df_insects <- filter(df_newveg1, class_model == "insect")

data.scores <- as.data.frame(scores(complete_insects))

data.scores$id <- rownames(complete_insects)
data.scores$veg <- df_insects$VegDescription2
data.scores$point <- df_insects$Point

head(data.scores$veg)

species.scores <- as.data.frame(scores(complete_insects, "species"))
species.scores$landvar <- rownames(species.scores)

head(species.scores)

p_completeinsects <- plot_ly()
p_completeinsects <- add_trace(p_completeinsects, name = data.scores$veg, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$point)
p_completeinsects <- add_trace(p_completeinsects, name = "Landscape attributes", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$landvar)
p_completeinsects <- layout(p_completeinsects, title = "Insects - all landscape variables (Stress:0.19)")

