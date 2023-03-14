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

complete_df <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  mutate_at(c(73:80, 87:101), scale) %>% 
  mutate(., period = case_when(hour > 4 & hour <= 11 ~ "morning",
                                                hour > 11 & hour <= 19 ~ "afternoon",
                                                T ~ "night"))
#Splittinng the df into bird and insects ####
#Bird analysis ##
bird_df <- filter(complete_df, class_model == "bird") %>% 
  droplevels(.)

#Insect analysis #

insect_df <- filter(complete_df, class_model == "insect") %>% 
  droplevels(.)

#Removing highly correlated variables ### removed: SAVI, mean humidity, nt height, ns height and litter####
# cor <- cor(bird_df[c(73:80, 87:101)]) %>% 
#   write.csv(getDataPath(chapter, "27.02.2021_birdcorrelation_landvariables.csv"))

#lm for mean and sd have low r squared so I'll try to group land variables and see if I can improve things - also maybe adding points to it?

lm.global <- lm(sd ~ CanopyCover +
                   ShrubCover +
                   CanopyHeight +
                   SubcanopyHeight +
                   Slope +
                   Aspect +
                   Elevation +
                   DistWater +
                   aug_ndvi_avg +
                   mean_temp +
                   NT_DIST_AVG +
                   NS_DIST_AVG +
                   GC_NG_AVG +
                   GC_NF_AVG +
                   GC_BS_AVG +
                   GC_SH_AVG + point + period + class_model,
                  data = complete_df)

summary(lm.global)

#NMDS to decrease number of land variables

library(vegan)
set.seed(123)

nmds_df <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  mutate(., period = case_when(hour > 4 & hour <= 11 ~ "morning",
                               hour > 11 & hour <= 19 ~ "afternoon",
                               T ~ "night"))

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

df_newveg1 <- select(land_var, Point, NewVegDescription, VegDescription2) %>% 
  merge(., nmds_df, by.x = "Point", by.y = "point") %>% 
  mutate_at(c(75:82, 89, 91, 95, 97, 99:103), decostand, method = "range")
  

rownames(df_newveg1) <- df_newveg1$Point 

# #####
# 
# 
# 
# bird_df <- filter(nmds_df, class_model == "bird") %>% 
#   group_by(point) %>% 
#   summarise(., mean_bird = mean(mean), sd_bird = mean(sd), mean_bird_temp = mean(mean_temp)) %>% 
#   merge(., land_var, by.x = "point", by.y = "Point", all.x = T, all.y = F) %>% 
#   filter(., point != "WAA2O" | point != "WBA2O") %>% 
#   #mutate_at(c(2:22), decostand, "range") %>% 
#   droplevels(.)
# 
# insect_df <- filter(nmds_df, class_model == "insect") %>% 
#   group_by(point) %>% 
#   summarise(., mean_insect = mean(mean), sd_insect = mean(sd), mean_insect_temp = mean(mean_temp)) %>% 
#   merge(., bird_df, by.x = "point", by.y = "point", all.x = T, all.y = T)  %>% 
#   filter(., point != "WAA2O" | point != "WBA2O") %>% 
#   mutate_at(c(2:22), decostand, "range") %>% 
#   droplevels(.)
# 
# #####
# 
# rownames(nmds_df) <- nmds_df$id

# df_test <- select(nmds_df, mean, sd, NT_DIST_AVG, SubcanopyHeight) %>% 
#   mutate_at(c(1:ncol(.)), decostand, "range") %>% 
#   droplevels(.)
#                   
#                   #NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG, aug_ndvi_avg, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, mean, sd) %>% 
#   
# 
# nmds_mean <- metaMDS(df_test, k = 2, trymax = 100)
# 
# nmds_mean
# stressplot(nmds_mean)
# 
# plot(nmds_mean)
# ordiplot(nmds_mean,type="n")
# ordihull(nmds_mean, groups=nmds_df$class_model, lty=2) col=cores2,
# #orditorp(nmds_mean,display="species",col="red",air=0.01)
# #orditorp(nmds_mean,display="sites",cex=0.9,air=0.01)
# 
# plot(nmds_mean, type="n")
# points(resultado.nmds, col=cores2[dados$Bloco_Amostral], pch=16)
# ordihull(resultado.nmds, groups=dados$Bloco_Amostral, col=cores2, lty=2)
# text(resultado.nmds, labels = row.names(bio), pos=4)
# 
# scores <- nmds_mean[["species"]]
# 
# adonis(nmds_mean[["dist"]]~nmds_df$class_model)

# nmds_df$aug_ndvi_avg <- as.numeric(nmds_df$aug_ndvi_avg)


#A PERMANOVA:

# colours <- c("#CCFF00", "#CCCC00",  "#CC9900", "#CC6600", "#CC3300",  "#FF00FF", "#660000", "#663399", "#666600", "#669900", "#66CC00", "#66FF00", "#009999", "#0066FF", "#000000")

# rownames(insect_df) <- insect_df$point
# 
# df_test <- select(insect_df, mean_insect, mean_bird, NT_DIST_AVG, NT_HEIGHT_AVG, CanopyHeight, Elevation)
# 
# nmds_mean <- metaMDS(df_test, k = 2, try = 100)
# 
# nmds_mean
# stressplot(nmds_mean)
# 
# plot(nmds_mean)
# ordiplot(nmds_mean,type="n")
# orditorp(nmds_mean,display="species",col="red",air=0.01)
# orditorp(nmds_mean,display="sites",cex=0.9,air=0.01)
# 
# scores <- nmds_mean[["species"]]
# 
# points(nmds_mean, col= colours[land_var$NewVegDescription], pch=16)
# #ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
# #legend("topleft", legend = as.factor(land_var$NewVegDescription), fill = colours, cex = 0.5)
# 
# scores <- nmds_mean[["species"]]
# 
# adonis(df_test~insect_df$NewVegDescription)
# anosim(df_test, insect_df$NewVegDescription)
# 
# ######
# 
# rownames(insect_df) <- insect_df$point
# 
# df_test <- select(insect_df, sd_insect, sd_bird, NT_DIST_AVG, NT_HEIGHT_AVG, CanopyHeight, Elevation)
# 
# nmds_sd <- metaMDS(df_test, k = 3, try = 100)
# 
# nmds_sd
# stressplot(nmds_sd)
# 
# plot(nmds_sd)
# ordiplot(nmds_sd,type="n")
# orditorp(nmds_sd,display="species",col="red",air=0.01)
# orditorp(nmds_sd,display="sites",cex=0.9,air=0.01)
# 
# points(nmds_sd, col= colours[insect_df$NewVegDescription], pch=16)
# #ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
# legend("topleft", legend = as.factor(insect_df$NewVegDescription), fill = colours, cex = 0.5)
# 
# scores <- nmds_sd[["species"]]
# 
# adonis(df_test~insect_df$NewVegDescription)
# anosim(df_test, insect_df$NewVegDescription)


#Complete model - birds and insects 

df_insects <- filter(df_newveg1, class_model == "insect")

row.names(df_newveg1) <- df_newveg1$id

colours <- c("#fdae61", "#8c510a",  "#b8e186", "#f46d43", "#4d9221")

dep_var_insects <- select(df_newveg1, mean_temp, SubcanopyHeight, Elevation, DistWater, GC_NF_AVG, Aspect)

all <- plyr::ldply(1:6, function(x)t(combn(colnames(dep_var_insects), x)))
all <- rename(all, col1 = 1, col2 = 2, col3 = 3, col4 = 4, col5 = 5, col6 = 6)

for (c in 1:ncol(all)) {
  all[,c] <- as.character(all[,c])
  
}

test <- as.list(all[seq(1,nrow(all), 16),])

scores <- data.frame( model_var = NA,
                      conv = NA,
                      stress = NA,
                      permanova_veg_F = NA,
                      permanova_veg_R2 = NA,
                      permanova_veg_p = NA,
                      permanova_class_F = NA,
                      permanova_class_R2 = NA,
                      permanova_class_p = NA)


scores_temp <- data.frame( model_var = NA,
                      conv = NA,
                      stress = NA,
                      permanova_veg_F = NA,
                      permanova_veg_R2 = NA,
                      permanova_veg_p = NA,
                      permanova_class_F = NA,
                      permanova_class_R2 = NA,
                      permanova_class_p = NA)

colours <- c("#fdae61", "#8c510a",  "#b8e186", "#f46d43", "#4d9221")
line_type <- c(5, 4, 3, 2, 1)
colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

summary(df_insects)

rm(outcome)

#jahs <- all[c(1, 1001, 10001, 20001, 30001, 40001, 50001, 60001, 65099, 65534),]

for (i in 1:nrow(all)){
  
  outcome <- NULL 
  model <- NULL
  PERMANOVA <- NULL
  perm <- NULL
  
    outcome <- as.character(all[i,]) %>% 
      na.exclude(.) %>% 
      paste(., sep = ",")
      
      skip_to_next <- FALSE
    
    tryCatch({
      model <- metaMDS(df_newveg1[outcome],
                        dist = "bray",
                        k = 2,
                        try = 100)

    
    PERMANOVA <- adonis(df_newveg1[outcome]~df_newveg1$VegDescription2)

      

png(filename=getDataPath("Chapter1_FineScaleAcousticSurvey", "Fig1", "NMDS_ALL_OPT", paste(rownames(all[i,]), "veg", ".png", sep = "")))
    
    plot(model)
    ordiplot(model,type="n")
    orditorp(model,display="sites",cex=0.9,air=0.01, labels = F)
    points(model, col= colours[df_newveg1$VegDescription2], pch=16)
    orditorp(model, display="species",col="red",air=0.5)
    ordihull(model, groups= df_newveg1$VegDescription2, lty = line_type)
    legend("topleft", legend = unique(df_newveg1$VegDescription2), fill = colours, cex = 0.6)
    legend("bottomleft", legend = unique(df_newveg1$VegDescription2), lty = line_type, cex = 0.6)
    
    dev.off()
    
    
#This one only for the complete models - with all groups
    
    # perm<-adonis(df_newveg[outcome]~df_newveg$class_model)
    # 
    # 
    # 
    # png(filename=getDataPath("Chapter1_FineScaleAcousticSurvey", "Fig1", "NMDS_insects", paste(rownames(all[i,]), "class", ".png", sep = "")))
    # 
    # plot(model)
    # ordiplot(model,type="n")
    # orditorp(model,display="sites",cex=0.9,air=0.01, labels = F)
    # points(model, col= colours2[df_newveg$class_model], pch=16)
    # ordihull(model, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
    # legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
    # legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)
    # dev.off()
    
    
    
      
      scores_temp$model_var <- as.character(rownames(all[i,]))
      scores_temp$conv <- as.character(model$converged)
      scores_temp$stress <- as.numeric(model$stress)
      scores_temp$permanova_veg_F <- as.numeric(PERMANOVA$aov.tab$F.Model[1])
      scores_temp$permanova_veg_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
      scores_temp$permanova_veg_p <- as.numeric(PERMANOVA$aov.tab$Pr[1])
      # scores_temp$permanova_class_F <- as.numeric(perm$aov.tab$F.Model[1])
      # scores_temp$permanova_class_R2 <- as.numeric(perm$aov.tab$R2[1])
      # scores_temp$permanova_class_p <- as.numeric(perm$aov.tab$Pr[1])
      
      write.csv(scores_temp, getDataPath("Chapter1_FineScaleAcousticSurvey", "Fig1", "NMDS_ALL_OPT", paste(rownames(all[i,]), "veg", ".csv", sep = ""))) },
      
      #scores <- rbind( scores_temp, scores) },

error = function(e) {skip_to_next <<-TRUE })
      
      if(skip_to_next) { next }
}
  

write.csv(all, getDataPath("Chapter1_FineScaleAcousticSurvey", "Fig1", "NMDS_INSECTS_OPT", "key_all_nmds_insects.csv")) 


#NMDS - complete model - no convergence ####

df_test <- select(df_newveg, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all <- metaMDS(df_test, k = 2, try = 100)

all

#Iterations

#No convergence####
df_test <- select(df_newveg, CanopyCover, ShrubCover) %>% 
  decostand(., method = "range") %>%  
  mutate(., replace(., is.na(.), 0)) %>% 
  droplevels(.)

iter1 <- metaMDS(df_test, k = 2, try = 100)

iter1

#No convergence####

df_test <- select(df_newveg, CanopyCover, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter2 <- metaMDS(df_test, k = 2, try = 100)

iter2

#No convergence####

df_test <- select(df_newveg, ShrubCover, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter3 <- metaMDS(df_test, k = 2, try = 100)

iter3

#No convergence####

df_test <- select(df_newveg, CanopyHeight, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter4 <- metaMDS(df_test, k = 2, try = 100)

iter4

#No convergence####

df_test <- select(df_newveg, SubcanopyHeight, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter5 <- metaMDS(df_test, k = 2, try = 100)

iter5

#Stress: 0.0003 - empty rows so dissimilarities may be meaningless####

df_test <- select(df_newveg, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter6 <- metaMDS(df_test, k = 2, try = 100)

iter6

#No convergence####

df_test <- select(df_newveg, Elevation, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter7 <- metaMDS(df_test, k = 2, try = 100)

iter7

#No convergence####

df_test <- select(df_newveg, Aspect, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

inter8 <- metaMDS(df_test, k = 2, try = 100)

inter8

#No convergence####

df_test <- select(df_newveg, DistWater, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter9 <- metaMDS(df_test, k = 2, try = 100)

iter9

#No convergence####

df_test <- select(df_newveg, aug_ndvi_avg, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter10 <- metaMDS(df_test, k = 2, try = 100)

iter10

#ITER 11 - Stress: 0.084805 - Dist water and Canopy Cover####

#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter11 <- metaMDS(df_test, k = 2, try = 100)

iter11

stressplot(iter11)

adonis(df_test~df_newveg$VegDescription2)

plot(iter11)
ordiplot(iter11,type="n")

orditorp(iter11,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter11, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter11, display="species",col="red",air=0.5)
ordihull(iter11, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter11)
ordiplot(iter11,type="n")

orditorp(iter11,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter11, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter11, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter11[["species"]]

#ITER 12 - Stress: 0.09964 - Dist water, Canopy Cover and Shrub cover####

#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter12 <- metaMDS(df_test, k = 2, try = 100)

iter12

stressplot(iter12)

adonis(df_test~df_newveg$VegDescription2)

plot(iter12)
ordiplot(iter12,type="n")

orditorp(iter12,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter12, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter12, display="species",col="red",air=0.5)
ordihull(iter12, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter12)
ordiplot(iter12,type="n")

orditorp(iter12,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter12, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter12, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter12[["species"]]


#ITER 13 - Stress: 0.099 - Dist water and Shrub cover####

#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter13 <- metaMDS(df_test, k = 2, try = 100)

iter13

stressplot(iter13)

adonis(df_test~df_newveg$VegDescription2)

plot(iter13)
ordiplot(iter13,type="n")

orditorp(iter13,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter13, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter13, display="species",col="red",air=0.5)
ordihull(iter13, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter13)
ordiplot(iter13,type="n")

orditorp(iter13,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter13, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter13, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter13[["species"]]

#
######
#ITER 14 - Stress: NO CONVERGENCE - Dist water, Shrub cover, Canopy Cover, Canopy Height####
#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover, CanopyHeight, CanopyCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter14 <- metaMDS(df_test, k = 2, try = 100)

iter14

stressplot(iter14)

adonis(df_test~df_newveg$VegDescription2)

plot(iter14)
ordiplot(iter14,type="n")

orditorp(iter14,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter14, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter14, display="species",col="red",air=0.5)
ordihull(iter14, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter14)
ordiplot(iter14,type="n")

orditorp(iter14,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter14, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter14, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter14[["species"]]

#ITER 15 - Stress: NO CONVERGENCE - Dist water, Shrub cover, Canopy Height####
#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover, CanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter15 <- metaMDS(df_test, k = 2, try = 100)

iter15

stressplot(iter15)

adonis(df_test~df_newveg$VegDescription2)

plot(iter15)
ordiplot(iter15,type="n")

orditorp(iter15,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter15, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter15, display="species",col="red",air=0.5)
ordihull(iter15, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter15)
ordiplot(iter15,type="n")

orditorp(iter15,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter15, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter15, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter15[["species"]]

#ITER 16 - Stress: NO CONVERGENCE - Dist water, Canopy cover, Canopy Height####
#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, CanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter16 <- metaMDS(df_test, k = 2, try = 100)

iter16

stressplot(iter16)

adonis(df_test~df_newveg$VegDescription2)

plot(iter16)
ordiplot(iter16,type="n")

orditorp(iter16,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter16, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter16, display="species",col="red",air=0.5)
ordihull(iter16, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter16)
ordiplot(iter16,type="n")

orditorp(iter16,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter16, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter16, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter16[["species"]]

#ITER 17 - Stress: NO CONVERGENCE - Dist water, Canopy Height####
#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter17 <- metaMDS(df_test, k = 2, try = 100)

iter17

stressplot(iter17)

adonis(df_test~df_newveg$VegDescription2)

plot(iter17)
ordiplot(iter17,type="n")

orditorp(iter17,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter17, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter17, display="species",col="red",air=0.5)
ordihull(iter17, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter17)
ordiplot(iter17,type="n")

orditorp(iter17,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter17, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter17, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter17[["species"]]

#
######
#ITER 18 - Stress: 0.1001728  - Dist water, CanopyCover, ShrubCover, SubcanopyHeight####
#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter18 <- metaMDS(df_test, k = 2, try = 100)

iter18

stressplot(iter18)

adonis(df_test~df_newveg$VegDescription2)

plot(iter18)
ordiplot(iter18,type="n")

orditorp(iter18,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter18, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter18, display="species",col="red",air=0.5)
ordihull(iter18, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter18)
ordiplot(iter18,type="n")

orditorp(iter18,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter18, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter18, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter18[["species"]]

#ITER 19 - Stress: 0.105613 - Dist water, CanopyCover, SubcanopyHeight####
#CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter19 <- metaMDS(df_test, k = 2, try = 100)

iter19

stressplot(iter19)

adonis(df_test~df_newveg$VegDescription2)

line_type <- c(5, 4, 3, 2, 1)

png(filename = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/Fig1/test.png")

 plot(iter19) 
ordiplot(iter19,type="n")

orditorp(iter19,display="sites",cex=0.9,air=0.01, labels = F) 




points(iter19, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter19, display="species",col="red",air=0.5)
ordihull(iter19, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

  
dev.off()

adonis(df_test~df_newveg$class_model)

plot(iter19)
ordiplot(iter19,type="n")

orditorp(iter19,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter19, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter19, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter19[["species"]]

#ITER 20 - Stress: 0.08245703 - Dist water, ShrubCover, SubcanopyHeight####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter20 <- metaMDS(df_test, k = 2, try = 100)

iter20

stressplot(iter20)

adonis(df_test~df_newveg$VegDescription2)

plot(iter20)
ordiplot(iter20,type="n")

orditorp(iter20,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter20, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter20, display="species",col="red",air=0.5)
ordihull(iter20, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter20)
ordiplot(iter20,type="n")

orditorp(iter20,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter20, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter20, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter20[["species"]]

#ITER 21 - Stress: 0.06952324 - Dist water, SubcanopyHeight####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter21 <- metaMDS(df_test, k = 2, try = 100)

iter21

stressplot(iter21)

adonis(df_test~df_newveg$VegDescription2)

plot(iter21)
ordiplot(iter21,type="n")

orditorp(iter21,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter21, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter21, display="species",col="red",air=0.5)
ordihull(iter21, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter21)
ordiplot(iter21,type="n")

orditorp(iter21,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter21, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter21, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter21[["species"]]


#
######
#ITER 22 - Stress: 0.1611949 - Dist water, CanopyCover, ShrubCover, SubcanopyHeight, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter22 <- metaMDS(df_test, k = 2, try = 100)

iter22

stressplot(iter22)

adonis(df_test~df_newveg$VegDescription2)

plot(iter22)
ordiplot(iter22,type="n")

orditorp(iter22,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter22, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter22, display="species",col="red",air=0.5)
ordihull(iter22, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter22)
ordiplot(iter22,type="n")

orditorp(iter22,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter22, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter22, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter22[["species"]]

#ITER 23 - Stress: 0.1178873 - Dist water, ShrubCover, SubcanopyHeight, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter23 <- metaMDS(df_test, k = 2, try = 100)

iter23

stressplot(iter23)

adonis(df_test~df_newveg$VegDescription2)

plot(iter23)
ordiplot(iter23,type="n")

orditorp(iter23,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter23, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter23, display="species",col="red",air=0.5)
ordihull(iter23, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter23)
ordiplot(iter23,type="n")

orditorp(iter23,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter23, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter23, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter23[["species"]]

#ITER 24 - Stress: 0.1700756 - Dist water, CanopyCover, SubcanopyHeight, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter24 <- metaMDS(df_test, k = 2, try = 100)

iter24

stressplot(iter24)

adonis(df_test~df_newveg$VegDescription2)

plot(iter24)
ordiplot(iter24,type="n")

orditorp(iter24,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter24, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter24, display="species",col="red",air=0.5)
ordihull(iter24, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter24)
ordiplot(iter24,type="n")

orditorp(iter24,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter24, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter24, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter24[["species"]]

#ITER 25 - Stress: NO CONVERGENCE - Dist water, CanopyCover, ShrubCover, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter25 <- metaMDS(df_test, k = 2, try = 100)

iter25

stressplot(iter25)

adonis(df_test~df_newveg$VegDescription2)

plot(iter25)
ordiplot(iter25,type="n")

orditorp(iter25,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter25, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter25, display="species",col="red",air=0.5)
ordihull(iter25, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter25)
ordiplot(iter25,type="n")

orditorp(iter25,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter25, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter25, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter25[["species"]]

#ITER 26 - Stress: 0.1090648 - Dist water, ShrubCover, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter26 <- metaMDS(df_test, k = 2, try = 100)

iter26

stressplot(iter26)

adonis(df_test~df_newveg$VegDescription2)

plot(iter26)
ordiplot(iter26,type="n")

orditorp(iter26,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter26, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter26, display="species",col="red",air=0.5)
ordihull(iter26, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter26)
ordiplot(iter26,type="n")

orditorp(iter26,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter26, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter26, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter26[["species"]]

#ITER 27 - Stress: 0.1091238 - Dist water, SubcanopyHeight, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter27 <- metaMDS(df_test, k = 2, try = 100)

iter27

stressplot(iter27)

adonis(df_test~df_newveg$VegDescription2)

plot(iter27)
ordiplot(iter27,type="n")

orditorp(iter27,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter27, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter27, display="species",col="red",air=0.5)
ordihull(iter27, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter27)
ordiplot(iter27,type="n")

orditorp(iter27,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter27, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter27, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter27[["species"]]

#ITER 28 - Stress: 0.1264407 - Dist water, CanopyCover, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter28 <- metaMDS(df_test, k = 2, try = 100)

iter28

stressplot(iter28)

adonis(df_test~df_newveg$VegDescription2)

plot(iter28)
ordiplot(iter28,type="n")

orditorp(iter28,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter28, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter28, display="species",col="red",air=0.5)
ordihull(iter28, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter28)
ordiplot(iter28,type="n")

orditorp(iter28,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter28, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter28, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter28[["species"]]


#ITER 29 - Stress: 0.05272172 - Dist water, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter29 <- metaMDS(df_test, k = 2, try = 100)

iter29

stressplot(iter29)

adonis(df_test~df_newveg$VegDescription2)

plot(iter29)
ordiplot(iter29,type="n")

orditorp(iter29,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter29, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter29, display="species",col="red",air=0.5)
ordihull(iter29, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter29)
ordiplot(iter29,type="n")

orditorp(iter29,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter29, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter29, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter29[["species"]]

#
######
#ITER 30 - Stress:  0.1654864 - Dist water, CanopyCover, ShrubCover, SubcanopyHeight, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover, SubcanopyHeight, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter30 <- metaMDS(df_test, k = 2, try = 100)

iter30

stressplot(iter30)

adonis(df_test~df_newveg$VegDescription2)

plot(iter30)
ordiplot(iter30,type="n")

orditorp(iter30,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter30, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter30, display="species",col="red",air=0.5)
ordihull(iter30, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter30)
ordiplot(iter30,type="n")

orditorp(iter30,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter30, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter30, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter30[["species"]]



#ITER 31 - Stress: 0.07009382 - Dist water, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter31 <- metaMDS(df_test, k = 2, try = 100)

iter31

stressplot(iter31)

adonis(df_test~df_newveg$VegDescription2)

plot(iter31)
ordiplot(iter31,type="n")

orditorp(iter31,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter31, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter31, display="species",col="red",air=0.5)
ordihull(iter31, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter31)
ordiplot(iter31,type="n")

orditorp(iter31,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter31, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter31, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter31[["species"]]


#ITER 32 - Stress: 0.1139706 - Dist water, SubcanopyHeight, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, SubcanopyHeight, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter32 <- metaMDS(df_test, k = 2, try = 100)

iter32

stressplot(iter32)

adonis(df_test~df_newveg$VegDescription2)

plot(iter32)
ordiplot(iter32,type="n")

orditorp(iter32,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter32, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter32, display="species",col="red",air=0.5)
ordihull(iter32, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter32)
ordiplot(iter32,type="n")

orditorp(iter32,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter32, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter32, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter32[["species"]]


#ITER 33 - Stress: NO CONVERGENCE - Dist water, CanopyCover, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter33 <- metaMDS(df_test, k = 2, try = 100)

iter33

stressplot(iter33)

adonis(df_test~df_newveg$VegDescription2)

plot(iter33)
ordiplot(iter33,type="n")

orditorp(iter33,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter33, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter33, display="species",col="red",air=0.5)
ordihull(iter33, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter33)
ordiplot(iter33,type="n")

orditorp(iter33,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter33, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter33, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter33[["species"]]


#ITER 34 - Stress: NO CONVERGENCE - Dist water, SubcanopyHeight, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater,SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter34 <- metaMDS(df_test, k = 2, try = 100)

iter34

stressplot(iter34)

adonis(df_test~df_newveg$VegDescription2)

plot(iter34)
ordiplot(iter34,type="n")

orditorp(iter34,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter34, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter34, display="species",col="red",air=0.5)
ordihull(iter34, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter34)
ordiplot(iter34,type="n")

orditorp(iter34,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter34, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter34, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter34[["species"]]


#ITER 35 - Stress: 0.1264276 - Dist water, ShrubCover, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter35 <- metaMDS(df_test, k = 2, try = 100)

iter35

stressplot(iter35)

adonis(df_test~df_newveg$VegDescription2)

plot(iter35)
ordiplot(iter35,type="n")

orditorp(iter35,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter35, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter35, display="species",col="red",air=0.5)
ordihull(iter35, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter35)
ordiplot(iter35,type="n")

orditorp(iter35,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter35, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter35, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter35[["species"]]

#ITER 36 - Stress: 0.1654857 - Dist water, ShrubCover, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover, SubcanopyHeight, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter36 <- metaMDS(df_test, k = 2, try = 100)

iter36

stressplot(iter36)

adonis(df_test~df_newveg$VegDescription2)

plot(iter36)
ordiplot(iter36,type="n")

orditorp(iter36,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter36, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter36, display="species",col="red",air=0.5)
ordihull(iter36, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter36)
ordiplot(iter36,type="n")

orditorp(iter36,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter36, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter36, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter36[["species"]]


#ITER 37 - Stress: 0.1654858 - Dist water, CanopyCover, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover, SubcanopyHeight, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter37 <- metaMDS(df_test, k = 2, try = 100)

iter37

stressplot(iter37)

adonis(df_test~df_newveg$VegDescription2)

plot(iter37)
ordiplot(iter37,type="n")

orditorp(iter37,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter37, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter37, display="species",col="red",air=0.5)
ordihull(iter37, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter37)
ordiplot(iter37,type="n")

orditorp(iter37,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter37, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter37, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter37[["species"]]


#ITER 38 - Stress: 0.05502336 - Dist water, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter38 <- metaMDS(df_test, k = 2, try = 100)

iter38

stressplot(iter38)

adonis(df_test~df_newveg$VegDescription2)

plot(iter38)
ordiplot(iter38,type="n")

orditorp(iter38,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter38, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter38, display="species",col="red",air=0.5)
ordihull(iter38, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter38)
ordiplot(iter38,type="n")

orditorp(iter38,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter38, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter38, groups= df_newveg$class_model, lty=line_type2)
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter38[["species"]]


#ITER 39 - Stress: 0.1000543 - Dist water, ShrubCover, SubcanopyHeight, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover, SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter39 <- metaMDS(df_test, k = 2, try = 100)

iter39

stressplot(iter39)

adonis(df_test~df_newveg$VegDescription2)

plot(iter39)
ordiplot(iter39,type="n")

orditorp(iter39,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter39, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter39, display="species",col="red",air=0.5)
ordihull(iter39, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter39)
ordiplot(iter39,type="n")

orditorp(iter39,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter39, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter39, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter39[["species"]]


#ITER 40 - Stress: NO CONVERGENCE - Dist water, CanopyCover, SubcanopyHeight, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter40 <- metaMDS(df_test, k = 2, try = 100)

iter40

stressplot(iter40)

adonis(df_test~df_newveg$VegDescription2)

plot(iter40)
ordiplot(iter40,type="n")

orditorp(iter40,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter40, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter40, display="species",col="red",air=0.5)
ordihull(iter40, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter40)
ordiplot(iter40,type="n")

orditorp(iter40,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter40, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter40, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter40[["species"]]

#ITER 41 - Stress: NO CONVERGENCE - Dist water, CanopyCover, ShrubCover, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter41 <- metaMDS(df_test, k = 2, try = 100)

iter41

stressplot(iter41)

adonis(df_test~df_newveg$VegDescription2)

plot(iter41)
ordiplot(iter41,type="n")

orditorp(iter41,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter41, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter41, display="species",col="red",air=0.5)
ordihull(iter41, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter41)
ordiplot(iter41,type="n")

orditorp(iter41,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter41, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter41, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter41[["species"]]

#ITER 73 - Stress:   - Dist water, Aspect, CanopyCover, ShrubCover, SubcanopyHeight####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Aspect, CanopyCover, ShrubCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter73 <- metaMDS(df_test, k = 2, try = 100)

iter73

stressplot(iter73)

adonis(df_test~df_newveg$VegDescription2)

plot(iter73)
ordiplot(iter73,type="n")

orditorp(iter73,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter73, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter73, display="species",col="red",air=0.5)
ordihull(iter73, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter73)
ordiplot(iter73,type="n")

orditorp(iter73,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter73, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter73, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter73[["species"]]

#ITER 74 - Stress:   - Dist water, Aspect, Slope, ShrubCover, SubcanopyHeight####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Aspect, CanopyCover, ShrubCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter74 <- metaMDS(df_test, k = 2, try = 100)

iter74

stressplot(iter74)

adonis(df_test~df_newveg$VegDescription2)

plot(iter74)
ordiplot(iter74,type="n")

orditorp(iter74,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter74, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter74, display="species",col="red",air=0.5)
ordihull(iter74, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter74)
ordiplot(iter74,type="n")

orditorp(iter74,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter74, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter74, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter74[["species"]]

#ITER 75 - Stress:   - Dist water, Aspect, Slope, CanopyCover, SubcanopyHeight####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Aspect, CanopyCover, ShrubCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter75 <- metaMDS(df_test, k = 2, try = 100)

iter75

stressplot(iter75)

adonis(df_test~df_newveg$VegDescription2)

plot(iter75)
ordiplot(iter75,type="n")

orditorp(iter75,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter75, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter75, display="species",col="red",air=0.5)
ordihull(iter75, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter75)
ordiplot(iter75,type="n")

orditorp(iter75,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter75, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter75, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter75[["species"]]

#ITER 76 - Stress:   - Dist water, Aspect, Slope, CanopyCover, ShrubCover####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Aspect, CanopyCover, ShrubCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter76 <- metaMDS(df_test, k = 2, try = 100)

iter76

stressplot(iter76)

adonis(df_test~df_newveg$VegDescription2)

plot(iter76)
ordiplot(iter76,type="n")

orditorp(iter76,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter76, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter76, display="species",col="red",air=0.5)
ordihull(iter76, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter76)
ordiplot(iter76,type="n")

orditorp(iter76,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter76, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter76, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter76[["species"]]

#####

#ITER 42 - Stress:   - Dist water, CanopyCover, ShrubCover, SubcanopyHeight, Slope, Aspect, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, CanopyCover, ShrubCover, SubcanopyHeight, Slope, Aspect, Elevation) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter42 <- metaMDS(df_test, k = 2, try = 100)

iter42

stressplot(iter42)

adonis(df_test~df_newveg$VegDescription2)

plot(iter42)
ordiplot(iter42,type="n")

orditorp(iter42,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter42, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter42, display="species",col="red",air=0.5)
ordihull(iter42, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter42)
ordiplot(iter42,type="n")

orditorp(iter42,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter42, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter42, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter42[["species"]]

#ITER 43 - Stress:   - Dist water, CanopyCover, Elevation ####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter43 <- metaMDS(df_test, k = 2, try = 100)

iter43

stressplot(iter43)

adonis(df_test~df_newveg$VegDescription2)

plot(iter43)
ordiplot(iter43,type="n")

orditorp(iter43,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter43, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter43, display="species",col="red",air=0.5)
ordihull(iter43, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter43)
ordiplot(iter43,type="n")

orditorp(iter43,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter43, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter43, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter43[["species"]]


#ITER 44 - Stress:   - Dist water, CanopyCover, ShrubCover, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter44 <- metaMDS(df_test, k = 2, try = 100)

iter44

stressplot(iter44)

adonis(df_test~df_newveg$VegDescription2)

plot(iter44)
ordiplot(iter44,type="n")

orditorp(iter44,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter44, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter44, display="species",col="red",air=0.5)
ordihull(iter44, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter44)
ordiplot(iter44,type="n")

orditorp(iter44,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter44, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter44, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter44[["species"]]

#ITER 45 - Stress:   - Dist water, SubcanopyHeight, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter45 <- metaMDS(df_test, k = 2, try = 100)

iter45

stressplot(iter45)

adonis(df_test~df_newveg$VegDescription2)

plot(iter45)
ordiplot(iter45,type="n")

orditorp(iter45,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter45, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter45, display="species",col="red",air=0.5)
ordihull(iter45, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter45)
ordiplot(iter45,type="n")

orditorp(iter45,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter45, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter45, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter45[["species"]]

#ITER 46 - Stress:   - Dist water, Slope, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter46 <- metaMDS(df_test, k = 2, try = 100)

iter46

stressplot(iter46)

adonis(df_test~df_newveg$VegDescription2)

plot(iter46)
ordiplot(iter46,type="n")

orditorp(iter46,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter46, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter46, display="species",col="red",air=0.5)
ordihull(iter46, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter46)
ordiplot(iter46,type="n")

orditorp(iter46,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter46, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter46, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter46[["species"]]

#ITER 47 - Stress:   - Dist water, Aspect, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter47 <- metaMDS(df_test, k = 2, try = 100)

iter47

stressplot(iter47)

adonis(df_test~df_newveg$VegDescription2)

plot(iter47)
ordiplot(iter47,type="n")

orditorp(iter47,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter47, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter47, display="species",col="red",air=0.5)
ordihull(iter47, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter47)
ordiplot(iter47,type="n")

orditorp(iter47,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter47, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter47, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter47[["species"]]

#ITER 48 - Stress:   - Dist water, CanopyCover, ShrubCover,  Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter48 <- metaMDS(df_test, k = 2, try = 100)

iter48

stressplot(iter48)

adonis(df_test~df_newveg$VegDescription2)

plot(iter48)
ordiplot(iter48,type="n")

orditorp(iter48,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter48, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter48, display="species",col="red",air=0.5)
ordihull(iter48, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter48)
ordiplot(iter48,type="n")

orditorp(iter48,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter48, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter48, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter48[["species"]]

#ITER 49 - Stress:   - Dist water, CanopyCover, SubcanopyHeight, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter49 <- metaMDS(df_test, k = 2, try = 100)

iter49

stressplot(iter49)

adonis(df_test~df_newveg$VegDescription2)

plot(iter49)
ordiplot(iter49,type="n")

orditorp(iter49,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter49, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter49, display="species",col="red",air=0.5)
ordihull(iter49, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter49)
ordiplot(iter49,type="n")

orditorp(iter49,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter49, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter49, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter49[["species"]]

#ITER 50 - Stress:   - Dist water, CanopyCover, Slope, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter50 <- metaMDS(df_test, k = 2, try = 100)

iter50

stressplot(iter50)

adonis(df_test~df_newveg$VegDescription2)

plot(iter50)
ordiplot(iter50,type="n")

orditorp(iter50,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter50, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter50, display="species",col="red",air=0.5)
ordihull(iter50, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter50)
ordiplot(iter50,type="n")

orditorp(iter50,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter50, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter50, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter50[["species"]]

#ITER 51 - Stress:   - Dist water, CanopyCover, Aspect, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter51 <- metaMDS(df_test, k = 2, try = 100)

iter51

stressplot(iter51)

adonis(df_test~df_newveg$VegDescription2)

plot(iter51)
ordiplot(iter51,type="n")

orditorp(iter51,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter51, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter51, display="species",col="red",air=0.5)
ordihull(iter51, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter51)
ordiplot(iter51,type="n")

orditorp(iter51,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter51, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter51, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter51[["species"]]

#ITER 52 - Stress:   - Dist water, ShrubCover, SubcanopyHeight, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, ShrubCover, SubcanopyHeight, Elevation) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter52 <- metaMDS(df_test, k = 2, try = 100)

iter52

stressplot(iter52)

adonis(df_test~df_newveg$VegDescription2)

plot(iter52)
ordiplot(iter52,type="n")

orditorp(iter52,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter52, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter52, display="species",col="red",air=0.5)
ordihull(iter52, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter52)
ordiplot(iter52,type="n")

orditorp(iter52,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter52, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter52, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter52[["species"]]

#ITER 53 - Stress:   - DistWater, Elevation, ShrubCover, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, ShrubCover, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter53 <- metaMDS(df_test, k = 2, try = 100)

iter53

stressplot(iter53)

adonis(df_test~df_newveg$VegDescription2)

plot(iter53)
ordiplot(iter53,type="n")

orditorp(iter53,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter53, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter53, display="species",col="red",air=0.5)
ordihull(iter53, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter53)
ordiplot(iter53,type="n")

orditorp(iter53,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter53, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter53, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter53[["species"]]

#ITER 54 - Stress:   - Dist water, ShrubCover, Aspect, Elevation####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, ShrubCover, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter54 <- metaMDS(df_test, k = 2, try = 100)

iter54

stressplot(iter54)

adonis(df_test~df_newveg$VegDescription2)

plot(iter54)
ordiplot(iter54,type="n")

orditorp(iter54,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter54, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter54, display="species",col="red",air=0.5)
ordihull(iter54, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter54)
ordiplot(iter54,type="n")

orditorp(iter54,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter54, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter54, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter54[["species"]]

#ITER 55 - Stress:   - Dist water, Elevation, SubcanopyHeight, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter55 <- metaMDS(df_test, k = 2, try = 100)

iter55

stressplot(iter55)

adonis(df_test~df_newveg$VegDescription2)

plot(iter55)
ordiplot(iter55,type="n")

orditorp(iter55,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter55, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter55, display="species",col="red",air=0.5)
ordihull(iter55, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter55)
ordiplot(iter55,type="n")

orditorp(iter55,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter55, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter55, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter55[["species"]]

#ITER 56 - Stress:   - Dist water, Elevation, SubcanopyHeight, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter56 <- metaMDS(df_test, k = 2, try = 100)

iter56

stressplot(iter56)

adonis(df_test~df_newveg$VegDescription2)

plot(iter56)
ordiplot(iter56,type="n")

orditorp(iter56,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter56, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter56, display="species",col="red",air=0.5)
ordihull(iter56, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter56)
ordiplot(iter56,type="n")

orditorp(iter56,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter56, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter56, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter56[["species"]]

#ITER 57 - Stress:   - Dist water, Elevation, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter57 <- metaMDS(df_test, k = 2, try = 100)

iter57

stressplot(iter57)

adonis(df_test~df_newveg$VegDescription2)

plot(iter57)
ordiplot(iter57,type="n")

orditorp(iter57,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter57, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter57, display="species",col="red",air=0.5)
ordihull(iter57, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter57)
ordiplot(iter57,type="n")

orditorp(iter57,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter57, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter57, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter57[["species"]]

#ITER 58 - Stress:   - Dist water, Elevation, CanopyCover, ShrubCover, SubcanopyHeight####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, ShrubCover, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter58 <- metaMDS(df_test, k = 2, try = 100)

iter58

stressplot(iter58)

adonis(df_test~df_newveg$VegDescription2)

plot(iter58)
ordiplot(iter58,type="n")

orditorp(iter58,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter58, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter58, display="species",col="red",air=0.5)
ordihull(iter58, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter58)
ordiplot(iter58,type="n")

orditorp(iter58,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter58, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter58, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter58[["species"]]

#ITER 59 - Stress:   - Dist water, Elevation, CanopyCover, ShrubCover, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, ShrubCover, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter59 <- metaMDS(df_test, k = 2, try = 100)

iter59

stressplot(iter59)

adonis(df_test~df_newveg$VegDescription2)

plot(iter59)
ordiplot(iter59,type="n")

orditorp(iter59,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter59, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter59, display="species",col="red",air=0.5)
ordihull(iter59, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter59)
ordiplot(iter59,type="n")

orditorp(iter59,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter59, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter59, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter59[["species"]]

#ITER 60 - Stress:   - Dist water, Elevation, CanopyCover, ShrubCover, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, ShrubCover, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter60 <- metaMDS(df_test, k = 2, try = 100)

iter60

stressplot(iter60)

adonis(df_test~df_newveg$VegDescription2)

plot(iter60)
ordiplot(iter60,type="n")

orditorp(iter60,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter60, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter60, display="species",col="red",air=0.5)
ordihull(iter60, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter60)
ordiplot(iter60,type="n")

orditorp(iter60,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter60, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter60, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter60[["species"]]

#ITER 61 - Stress:   - Dist water, Elevation, CanopyCover, SubcanopyHeight, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter61 <- metaMDS(df_test, k = 2, try = 100)

iter61

stressplot(iter61)

adonis(df_test~df_newveg$VegDescription2)

plot(iter61)
ordiplot(iter61,type="n")

orditorp(iter61,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter61, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter61, display="species",col="red",air=0.5)
ordihull(iter61, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter61)
ordiplot(iter61,type="n")

orditorp(iter61,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter61, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter61, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter61[["species"]]

#ITER 62 - Stress:   - Dist water, Elevation, CanopyCover, SubcanopyHeight, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter62 <- metaMDS(df_test, k = 2, try = 100)

iter62

stressplot(iter62)

adonis(df_test~df_newveg$VegDescription2)

plot(iter62)
ordiplot(iter62,type="n")

orditorp(iter62,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter62, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter62, display="species",col="red",air=0.5)
ordihull(iter62, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter62)
ordiplot(iter62,type="n")

orditorp(iter62,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter62, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter62, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter62[["species"]]

#ITER 63 - Stress:   - Dist water, Elevation, CanopyCover, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyCover, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter63 <- metaMDS(df_test, k = 2, try = 100)

iter63

stressplot(iter63)

adonis(df_test~df_newveg$VegDescription2)

plot(iter63)
ordiplot(iter63,type="n")

orditorp(iter63,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter63, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter63, display="species",col="red",air=0.5)
ordihull(iter63, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter63)
ordiplot(iter63,type="n")

orditorp(iter63,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter63, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter63, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter63[["species"]]

#ITER 64 - Stress:   - Dist water, Elevation, ShrubCover, SubcanopyHeight, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, ShrubCover, SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter64 <- metaMDS(df_test, k = 2, try = 100)

iter64

stressplot(iter64)

adonis(df_test~df_newveg$VegDescription2)

plot(iter64)
ordiplot(iter64,type="n")

orditorp(iter64,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter64, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter64, display="species",col="red",air=0.5)
ordihull(iter64, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter64)
ordiplot(iter64,type="n")

orditorp(iter64,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter64, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter64, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter64[["species"]]

#ITER 65 - Stress:   - Dist water, Elevation, ShrubCover, SubcanopyHeight, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, ShrubCover, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter65 <- metaMDS(df_test, k = 2, try = 100)

iter65

stressplot(iter65)

adonis(df_test~df_newveg$VegDescription2)

plot(iter65)
ordiplot(iter65,type="n")

orditorp(iter65,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter65, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter65, display="species",col="red",air=0.5)
ordihull(iter65, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter65)
ordiplot(iter65,type="n")

orditorp(iter65,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter65, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter65, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter65[["species"]]

#ITER 66 - Stress:   - Dist water, Elevation, ShrubCover, Aspect, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, ShrubCover, Aspect, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter66 <- metaMDS(df_test, k = 2, try = 100)

iter66

stressplot(iter66)

adonis(df_test~df_newveg$VegDescription2)

plot(iter66)
ordiplot(iter66,type="n")

orditorp(iter66,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter66, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter66, display="species",col="red",air=0.5)
ordihull(iter66, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter66)
ordiplot(iter66,type="n")

orditorp(iter66,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter66, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter66, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter66[["species"]]

#ITER 67 - Stress:   - Dist water, Elevation, SubcanopyHeight, Aspect, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, SubcanopyHeight, Aspect, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter67 <- metaMDS(df_test, k = 2, try = 100)

iter67

stressplot(iter67)

adonis(df_test~df_newveg$VegDescription2)

plot(iter67)
ordiplot(iter67,type="n")

orditorp(iter67,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter67, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter67, display="species",col="red",air=0.5)
ordihull(iter67, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter67)
ordiplot(iter67,type="n")

orditorp(iter67,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter67, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter67, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter67[["species"]]

#ITER 68 - Stress:   - Dist water, Elevation, CanopyHeight, ShrubCover, SubcanopyHeight, Slope####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyHeight, ShrubCover, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter68 <- metaMDS(df_test, k = 2, try = 100)

iter68

stressplot(iter68)

adonis(df_test~df_newveg$VegDescription2)

plot(iter68)
ordiplot(iter68,type="n")

orditorp(iter68,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter68, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter68, display="species",col="red",air=0.5)
ordihull(iter68, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter68)
ordiplot(iter68,type="n")

orditorp(iter68,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter68, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter68, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter68[["species"]]

#ITER 69 - Stress:   - Dist water, Elevation, CanopyHeight, ShrubCover, SubcanopyHeight, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, CanopyHeight, ShrubCover, SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter69 <- metaMDS(df_test, k = 2, try = 100)

iter69

stressplot(iter69)

adonis(df_test~df_newveg$VegDescription2)

plot(iter69)
ordiplot(iter69,type="n")

orditorp(iter69,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter69, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter69, display="species",col="red",air=0.5)
ordihull(iter69, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter69)
ordiplot(iter69,type="n")

orditorp(iter69,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter69, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter69, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter69[["species"]]

#ITER 70 - Stress:   - Dist water, Elevation, ShrubCover, SubcanopyHeight, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, Slope, ShrubCover, SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter70 <- metaMDS(df_test, k = 2, try = 100)

iter70

stressplot(iter70)

adonis(df_test~df_newveg$VegDescription2)

plot(iter70)
ordiplot(iter70,type="n")

orditorp(iter70,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter70, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter70, display="species",col="red",air=0.5)
ordihull(iter70, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter70)
ordiplot(iter70,type="n")

orditorp(iter70,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter70, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter70, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter70[["species"]]

#ITER 71 - Stress:   - Dist water, Elevation, CanopyCover, SubcanopyHeight, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, Slope, CanopyCover, SubcanopyHeight, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter71 <- metaMDS(df_test, k = 2, try = 100)

iter71

stressplot(iter71)

test1<- adonis(df_test~df_newveg$VegDescription2)

plot(iter71)
ordiplot(iter71,type="n")

orditorp(iter71,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter71, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter71, display="species",col="red",air=0.5)
ordihull(iter71, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter71)
ordiplot(iter71,type="n")

orditorp(iter71,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter71, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter71, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter71[["species"]]

#ITER 72 - Stress:   - Dist water, Elevation, CanopyCover, ShrubCover, Slope, Aspect####
#CanopyCover, ShrubCover, CanopyHeight - no convergence, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG

df_test <- select(df_newveg, DistWater, Elevation, Slope, CanopyCover, ShrubCover, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter72 <- metaMDS(df_test, k = 2, try = 100)

iter72

stressplot(iter72)

adonis(df_test~df_newveg$VegDescription2)

plot(iter72)
ordiplot(iter72,type="n")

orditorp(iter72,display="sites",cex=0.9,air=0.01, labels = F)


line_type <- c(5, 4, 3, 2, 1)

points(iter72, col= colours[df_newveg$VegDescription2], pch=16)
orditorp(iter72, display="species",col="red",air=0.5)
ordihull(iter72, groups= df_newveg$VegDescription2, lty = line_type)
legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)

adonis(df_test~df_newveg$class_model)

plot(iter72)
ordiplot(iter72,type="n")

orditorp(iter72,display="sites",cex=0.9,air=0.01, labels = F)

colours2 <- c("#542788", "#b35806")
line_type2 <- c(1, 2)

points(iter72, col= colours2[df_newveg$class_model], pch=16)
ordihull(iter72, groups= df_newveg$class_model, lty=line_type2[df_newveg$class_model])
legend("topleft", legend = unique(df_newveg$class_model), fill = colours2, cex = 0.6)
legend("bottomleft", legend = unique(df_newveg$class_model), lty = line_type2, cex = 0.6)

scores <- iter72[["species"]]

#
#
#No convergence####

df_test <- select(df_newveg, NT_DIST_AVG, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter12 <- metaMDS(df_test, k = 2, try = 100)

iter12

#Stress: 0.000141####

df_test <- select(df_newveg, NS_DIST_AVG, GC_NG_AVG, mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter13 <- metaMDS(df_test, k = 2, try = 100)

iter13

#Stress: 9.288635e-05####

df_test <- select(df_newveg, GC_NG_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter14 <- metaMDS(df_test, k = 2, try = 100)

iter14

#No convergence####

df_test <- select(df_newveg, GC_NF_AVG, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter15 <- metaMDS(df_test, k = 2, try = 100)

iter15

#No convergence####

df_test <- select(df_newveg, GC_BS_AVG, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter16 <- metaMDS(df_test, k = 2, try = 100)

iter16

#No convergence####

df_test <- select(df_newveg, GC_SH_AVG, mean, sd) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

iter17 <- metaMDS(df_test, k = 2, try = 100)

iter17

#NMDS - canopy + water + temp model - no convergence ####

df_test <- select(df_newveg, CanopyCover, CanopyHeight, aug_ndvi_avg, mean_temp, NT_DIST_AVG, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#NMDS - canopy and temperature model - no convergence####

df_test <- select(df_newveg, CanopyCover, CanopyHeight, aug_ndvi_avg, mean_temp, NT_DIST_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd


#NMDS - canopy and dist to water ####

df_test <- select(df_newveg, CanopyCover, CanopyHeight, aug_ndvi_avg, NT_DIST_AVG, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#Shrub + dist to water + temp
df_test <- select(df_newveg, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg, DistWater, mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#Shrub and temp model
df_test <- select(df_newveg, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg,  mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#Shrub and dist to water
df_test <- select(df_newveg, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#Shrub only model
df_test <- select(df_newveg, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#DEM model + Dist water + temp

df_test <- select(df_newveg, Slope, Aspect, Elevation, mean_temp, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#DEM model + dist water

df_test <- select(df_newveg, Slope, Aspect, Elevation, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#DEM model + temp

df_test <- select(df_newveg, Slope, Aspect, Elevation, mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#DEM model

df_test <- select(df_newveg, Slope, Aspect, Elevation) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_dem <- metaMDS(df_test, k = 2, try = 100)

all_dem

stressplot(all_dem)

plot(all_dem)
ordiplot(all_dem,type="n")
orditorp(all_dem,display="species",col="red",air=0.01)
orditorp(all_dem,display="sites",cex=0.9,air=0.01, labels = F)

points(all_dem, col= colours[df_newveg$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topleft", legend = unique(df_newveg$NewVegDescription), fill = colours, cex = 0.6)

points(all_dem, col= colours[df_newveg$class_model], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topleft", legend = unique(df_newveg$class_model), fill = colours, cex = 0.6)

scores <- all_dem[["species"]]

adonis(df_test~df_newveg$class_model)
anosim(df_test, df_newveg$class_model)
adonis(df_test~df_newveg$NewVegDescription)
anosim(df_test, df_newveg$NewVegDescription)

#Ground Cover models

#Ground cover, temp and dist to water
df_test <- select(df_newveg, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#Ground cover and temp
df_test <- select(df_newveg, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#Ground cover and dist to water
df_test <- select(df_newveg, DistWater, aug_ndvi_avg, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_sd <- metaMDS(df_test, k = 2, try = 100)

nmds_sd

#Ground cover only
df_test <- select(df_newveg, aug_ndvi_avg, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_vegonly <- metaMDS(df_test, k = 2, try = 100)

all_vegonly

stressplot(all_dem)

plot(all_dem)
ordiplot(all_dem,type="n")
orditorp(all_dem,display="species",col="red",air=0.01)
orditorp(all_dem,display="sites",cex=0.9,air=0.01, labels = F)

points(all_dem, col= colours[df_newveg$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topleft", legend = unique(df_newveg$NewVegDescription), fill = colours, cex = 0.6)

points(all_dem, col= colours[df_newveg$class_model], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topleft", legend = unique(df_newveg$class_model), fill = colours, cex = 0.6)

scores <- all_dem[["species"]]

adonis(df_test~df_newveg$class_model)
anosim(df_test, df_newveg$class_model)
adonis(df_test~df_newveg$NewVegDescription)
anosim(df_test, df_newveg$NewVegDescription)

#Iterative models --> NO CONVERGENCE
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter1 <- metaMDS(df_test, k = 2, try = 100)

all_iter1

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter2 <- metaMDS(df_test, k = 2, try = 100)

all_iter2

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, CanopyCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter3 <- metaMDS(df_test, k = 2, try = 100)

all_iter3

stressplot(all_iter3)

plot(all_iter3)
ordiplot(all_iter3,type="n")
orditorp(all_iter3,display="species",col="red",air=0.01)
orditorp(all_iter3,display="sites",cex=0.9,air=0.01, labels = F)

points(all_iter3, col= colours[df_newveg$class_model], pch=16)
legend("topleft", legend = unique(df_newveg$class_model), fill = colours, cex = 0.6)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
points(all_iter3, col= colours[df_newveg$NewVegDescription], pch=16)
legend("bottomleft", legend = unique(df_newveg$NewVegDescription), fill = colours, cex = 0.6)

scores <- all_iter3[["species"]]

adonis(df_test~df_newveg$NewVegDescription)
anosim(df_test, df_newveg$NewVegDescription)
adonis(df_test~df_newveg$class_model)
anosim(df_test, df_newveg$class_model)

#Iterative models
df_test <- select(df_newveg, NS_DIST_AVG, CanopyCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter4 <- metaMDS(df_test, k = 2, try = 100)

all_iter4
adonis(df_test~df_newveg$class_model)
adonis(df_test~df_newveg$NewVegDescription)

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter5 <- metaMDS(df_test, k = 2, try = 100)

all_iter5

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter6 <- metaMDS(df_test, k = 2, try = 100)

all_iter6

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter7 <- metaMDS(df_test, k = 2, try = 100)

all_iter7

#Iterative models
df_test <- select(df_newveg, NS_DIST_AVG, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter8 <- metaMDS(df_test, k = 2, try = 100)

all_iter8

#Iterative models
df_test <- select(df_newveg, CanopyCover, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter9 <- metaMDS(df_test, k = 2, try = 100)

all_iter9

#Iterative models
df_test <- select(df_newveg, NS_DIST_AVG, CanopyCover, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter10 <- metaMDS(df_test, k = 2, try = 100)

all_iter10

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, CanopyCover, ShrubCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter11 <- metaMDS(df_test, k = 2, try = 100)

all_iter11

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter12 <- metaMDS(df_test, k = 2, try = 100)

all_iter12

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter13 <- metaMDS(df_test, k = 2, try = 100)

all_iter13

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter14 <- metaMDS(df_test, k = 2, try = 100)

all_iter14

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter15 <- metaMDS(df_test, k = 2, try = 100)

all_iter15

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter16 <- metaMDS(df_test, k = 2, try = 100)

all_iter16

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter17 <- metaMDS(df_test, k = 2, try = 100)

all_iter17

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter18 <- metaMDS(df_test, k = 2, try = 100)

all_iter18

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter19 <- metaMDS(df_test, k = 2, try = 100)

all_iter19

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, GC_NG_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter20 <- metaMDS(df_test, k = 2, try = 100)

all_iter20

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, GC_NG_AVG, GC_NF_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter21 <- metaMDS(df_test, k = 2, try = 100)

all_iter21

#Iterative models
df_test <- select(df_newveg, NT_DIST_AVG, NS_DIST_AVG, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

all_iter22 <- metaMDS(df_test, k = 2, try = 100)

all_iter22

#Insects ####
df_filtered <- filter(df_newveg, class_model == "insect")

#Complete model - birds and insects 

row.names(df_filtered) <- df_filtered$id

colours <- c("#CCFF00", "#CCCC00",  "#CC9900", "#CC6600", "#CC3300",  "#FF00FF", "#660000", "#663399", "#666600", "#669900", "#66CC00", "#66FF00", "#009999", "#0066FF", "#000000")

#NMDS - complete model - no convergence ####

df_test <- select(df_filtered, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_complete <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_complete


#NMDS - canopy + water + temp model - no convergence ####

df_test <- select(df_filtered, CanopyCover, CanopyHeight, aug_ndvi_avg, mean_temp, NT_DIST_AVG, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_canopy1 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_canopy1

#NMDS - canopy and temperature model - no convergence####

df_test <- select(df_filtered, CanopyCover, CanopyHeight, aug_ndvi_avg, mean_temp, NT_DIST_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_canopy2 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_canopy2


#NMDS - canopy and dist to water ####

df_test <- select(df_filtered, CanopyCover, CanopyHeight, aug_ndvi_avg, NT_DIST_AVG, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_canopy3 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_canopy3

#NMDS - canopy only ####

df_test <- select(df_filtered, CanopyCover, CanopyHeight, aug_ndvi_avg, NT_DIST_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_canopy4 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_canopy4

plot(nmds_insects_canopy4)
ordiplot(nmds_insects_canopy4,type="n")
orditorp(nmds_insects_canopy4,display="species",col="red",air=0.01)
orditorp(nmds_insects_canopy4,display="sites",cex=0.9,air=0.01, labels = F)

points(nmds_insects_canopy4, col= colours[df_filtered$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topright", legend = unique(df_filtered$NewVegDescription), fill = colours, cex = 0.5)

scores <- nmds_insects_canopy4[["species"]]

adonis(df_test~df_filtered$NewVegDescription)
anosim(df_test, df_filtered$NewVegDescription)

#Shrub + dist to water + temp####
df_test <- select(df_filtered, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg, DistWater, mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_shrub1 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_shrub1

#Shrub and temp model####
df_test <- select(df_filtered, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg,  mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_shrub2 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_shrub2

#Shrub and dist to water####
df_test <- select(df_filtered, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_shrub3 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_shrub3

#Shrub only model####
df_test <- select(df_filtered, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_shrub4 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_shrub4

#DEM model + Dist water + temp####

df_test <- select(df_filtered, Slope, Aspect, Elevation, mean_temp, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_dem1 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_dem1

#DEM model + dist water####

df_test <- select(df_filtered, Slope, Aspect, Elevation, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_dem2 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_dem2

#DEM model + temp####

df_test <- select(df_filtered, Slope, Aspect, Elevation, mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_dem3 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_dem3

#DEM model####

df_test <- select(df_filtered, Slope, Aspect, Elevation) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_dem4 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_dem4

plot(nmds_insects_dem4)
ordiplot(nmds_insects_dem4,type="n")
orditorp(nmds_insects_dem4,display="species",col="red",air=0.01)
orditorp(nmds_insects_dem4,display="sites",cex=0.9,air=0.01, labels = F)

points(nmds_insects_dem4, col= colours[df_filtered$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topright", legend = unique(df_filtered$NewVegDescription), fill = colours, cex = 0.5)

scores <- nmds_insects_dem4[["species"]]

adonis(df_test~df_filtered$NewVegDescription)
anosim(df_test, df_filtered$NewVegDescription)

#Ground Cover models####

#Ground cover, temp and dist to water####
df_test <- select(df_filtered, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_ground1 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_ground1

#Ground cover and temp####
df_test <- select(df_filtered, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_ground2 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_ground2

#Ground cover and dist to water####
df_test <- select(df_filtered, DistWater, aug_ndvi_avg, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_ground3 <- metaMDS(df_test, k = 2, try = 100)

nmds_insects_ground3

#Ground cover only####
df_test <- select(df_filtered, aug_ndvi_avg, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_insects_ground4 <- metaMDS(df_test, k = 2, m.try = 100)

nmds_insects_ground4

plot(nmds_insects_ground4)
ordiplot(nmds_insects_ground4,type="n")
orditorp(nmds_insects_ground4,display="species",col="red",air=0.01)
orditorp(nmds_insects_ground4,display="sites",cex=0.9,air=0.01, labels = F)

points(nmds_insects_ground4, col= colours[df_filtered$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topright", legend = unique(df_filtered$NewVegDescription), fill = colours, cex = 0.5)

scores <- nmds_insects_ground4[["species"]]

adonis(df_test~df_filtered$NewVegDescription)
anosim(df_test, df_filtered$NewVegDescription)

#Iterative models####
df_test <- select(df_filtered, CanopyCover) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_iter1 <- metaMDS(df_test, k = 2, try = 100)

nmds_iter1

#Birds ####
df_filtered <- filter(df_newveg, class_model == "bird")

#Complete model - birds and insects 

row.names(df_filtered) <- df_filtered$id

colours <- c("#CCFF00", "#CCCC00",  "#CC9900", "#CC6600", "#CC3300",  "#FF00FF", "#660000", "#663399", "#666600", "#669900", "#66CC00", "#66FF00", "#009999", "#0066FF", "#000000")

#NMDS - complete model -  ####

df_test <- select(df_filtered, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_complete <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_complete

plot(nmds_bird_complete)
ordiplot(nmds_bird_complete,type="n")
orditorp(nmds_bird_complete,display="species",col="red",air=0.01)
orditorp(nmds_bird_complete,display="sites",cex=0.9,air=0.01, labels = F)

points(nmds_bird_complete, col= colours[df_filtered$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("bottomright", legend = unique(df_filtered$NewVegDescription), fill = colours, cex = 0.5)

scores <- nmds_bird_complete[["species"]]

adonis(df_test~df_filtered$NewVegDescription)
anosim(df_test, df_filtered$NewVegDescription)


#NMDS - canopy + water + temp model - no convergence ####

df_test <- select(df_filtered, CanopyCover, CanopyHeight, aug_ndvi_avg, mean_temp, NT_DIST_AVG, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_canopy1 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_canopy1

#NMDS - canopy and temperature model - no convergence####

df_test <- select(df_filtered, CanopyCover, CanopyHeight, aug_ndvi_avg, mean_temp, NT_DIST_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_canopy2 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_canopy2


#NMDS - canopy and dist to water ####

df_test <- select(df_filtered, CanopyCover, CanopyHeight, aug_ndvi_avg, NT_DIST_AVG, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_canopy3 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_canopy3

#NMDS - canopy only ####

df_test <- select(df_filtered, CanopyCover, CanopyHeight, aug_ndvi_avg, NT_DIST_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_canopy4 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_canopy4

plot(nmds_bird_canopy4)
ordiplot(nmds_bird_canopy4,type="n")
orditorp(nmds_bird_canopy4,display="species",col="red",air=0.01)
orditorp(nmds_bird_canopy4,display="sites",cex=0.9,air=0.01, labels = F)

points(nmds_bird_canopy4, col= colours[df_filtered$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topright", legend = unique(df_filtered$NewVegDescription), fill = colours, cex = 0.5)

scores <- nmds_bird_canopy4[["species"]]

adonis(df_test~df_filtered$NewVegDescription)
anosim(df_test, df_filtered$NewVegDescription)

#Shrub + dist to water + temp####
df_test <- select(df_filtered, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg, DistWater, mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_shrub1 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_shrub1

#Shrub and temp model####
df_test <- select(df_filtered, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg,  mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_shrub2 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_shrub2

#Shrub and dist to water####
df_test <- select(df_filtered, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_shrub3 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_shrub3

#Shrub only model####
df_test <- select(df_filtered, ShrubCover, SubcanopyHeight, NS_DIST_AVG, GC_SH_AVG, aug_ndvi_avg) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_shrub4 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_shrub4

#DEM model + Dist water + temp####

df_test <- select(df_filtered, Slope, Aspect, Elevation, mean_temp, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_dem1 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_dem1

#DEM model + dist water####

df_test <- select(df_filtered, Slope, Aspect, Elevation, DistWater) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_dem2 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_dem2

#DEM model + temp####

df_test <- select(df_filtered, Slope, Aspect, Elevation, mean_temp) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_dem3 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_dem3

#DEM model####

df_test <- select(df_filtered, Slope, Aspect, Elevation) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_dem4 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_dem4

plot(nmds_bird_dem4)
ordiplot(nmds_bird_dem4,type="n")
orditorp(nmds_bird_dem4,display="species",col="red",air=0.01)
orditorp(nmds_bird_dem4,display="sites",cex=0.9,air=0.01, labels = F)

points(nmds_bird_dem4, col= colours[df_filtered$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("bottomright", legend = unique(df_filtered$NewVegDescription), fill = colours, cex = 0.5)

scores <- nmds_bird_dem4[["species"]]

adonis(df_test~df_filtered$NewVegDescription)
anosim(df_test, df_filtered$NewVegDescription)

#Ground Cover models####

#Ground cover, temp and dist to water####
df_test <- select(df_filtered, DistWater, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_ground1 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_ground1

#Ground cover and temp####
df_test <- select(df_filtered, aug_ndvi_avg, mean_temp, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_ground2 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_ground2

#Ground cover and dist to water####
df_test <- select(df_filtered, DistWater, aug_ndvi_avg, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_ground3 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_ground3

#Ground cover only####
df_test <- select(df_filtered, aug_ndvi_avg, NT_DIST_AVG, NS_DIST_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG) %>% 
  decostand(., method = "range") %>% 
  droplevels(.)

nmds_bird_ground4 <- metaMDS(df_test, k = 2, try = 100)

nmds_bird_ground4

plot(nmds_bird_ground4)
ordiplot(nmds_bird_ground4,type="n")
orditorp(nmds_bird_ground4,display="species",col="red",air=0.01)
orditorp(nmds_bird_ground4,display="sites",cex=0.9,air=0.01, labels = F)

points(nmds_bird_ground4, col= colours[df_filtered$NewVegDescription], pch=16)
#ordihull(nmds_mean, groups= land_var$NewVegDescription, lty=2)
legend("topleft", legend = unique(df_filtered$NewVegDescription), fill = colours, cex = 0.6)

scores <- nmds_bird_ground4[["species"]]

adonis(df_test~df_filtered$NewVegDescription)
anosim(df_test, df_filtered$NewVegDescription)

#Iterative models