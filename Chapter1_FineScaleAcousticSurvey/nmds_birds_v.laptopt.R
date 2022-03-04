#NMDS#
#Marina Scarpelli#
#07.01.2020#

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(stringi)
library(car)
library(data.table)
library(MuMIn)
library(plotly)

#Reading and preparing the data ####
getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"


#NMDS to decrease number of land variables

library(vegan)
set.seed(123)

nmds_df <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  mutate(., period = case_when(hour > 4 & hour <= 11 ~ "morning",
                               hour > 11 & hour <= 19 ~ "afternoon",
                               T ~ "night"))

land_var <- read.csv(getDataPath("Fieldwork_Bowra", "26.02.2021_dataSAVINDVI_v.laptop.csv"))

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


rownames(land_var) <- land_var$Point 

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

df_newveg <- filter(df_newveg1, class_model == "bird")

row.names(df_newveg) <- df_newveg$id

colours <- c("#fdae61", "#8c510a",  "#b8e186", "#f46d43", "#4d9221")

dep_var <- select(df_newveg, mean_temp, SubcanopyHeight, DistWater, aug_ndvi_avg, Elevation, GC_NF_AVG)

all <- plyr::ldply(1:6, function(x)t(combn(colnames(dep_var), x)))
all <- rename(all, col1 = 1, col2 = 2, col3 = 3, col4 = 4, col5 = 5, col6 = 6)

for (c in 1:ncol(all)) {
  all[,c] <- as.character(all[,c])
  
}

write.csv(all, getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT", "key_birds_nmds_take2.csv")) 


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

summary(df_newveg)

df_newveg$VegDescription2 <- as.factor(df_newveg$VegDescription2)

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
    model <- metaMDS(df_newveg[outcome],
                     dist = "bray",
                     k = 2,
                     try = 100)
    
    
    PERMANOVA <- adonis(df_newveg[outcome]~df_newveg$VegDescription2)
    
    
    
    png(filename=getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT", paste(rownames(all[i,]), "veg_TAKE2", ".png", sep = "")))
    
    plot(model)
    ordiplot(model,type="n")
    orditorp(model,display="sites",cex=0.9,air=0.01, labels = F)
    points(model, col= colours[df_newveg$VegDescription2], pch = 16)
    orditorp(model, display="species",col="red",air=0.5)
    ordihull(model, groups= df_newveg$VegDescription2, lty = line_type)
    legend("topleft", legend = unique(df_newveg$VegDescription2), fill = colours, cex = 0.6)
    legend("bottomleft", legend = unique(df_newveg$VegDescription2), lty = line_type, cex = 0.6)
    
    dev.off()
    
    
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
    
    
    
    
    scores_temp$model_var <- as.character(rownames(all[i,]))
    scores_temp$conv <- as.character(model$converged)
    scores_temp$stress <- as.numeric(model$stress)
    scores_temp$permanova_veg_F <- as.numeric(PERMANOVA$aov.tab$F.Model[1])
    scores_temp$permanova_veg_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
    scores_temp$permanova_veg_p <- as.numeric(PERMANOVA$aov.tab$Pr[1])
    # scores_temp$permanova_class_F <- as.numeric(perm$aov.tab$F.Model[1])
    # scores_temp$permanova_class_R2 <- as.numeric(perm$aov.tab$R2[1])
    # scores_temp$permanova_class_p <- as.numeric(perm$aov.tab$Pr[1])
    
    write.csv(scores_temp, getDataPath("Chapter1_FineScaleAcousticSurvey", "Fig1", "NMDS_BIRDS_OPT", paste(rownames(all[i,]), "veg_TAKE2", ".csv", sep = ""))) },
    
    #scores <- rbind(scores, scores_temp) },
    
    error = function(e) {skip_to_next <<-TRUE })
  
  if(skip_to_next) { next }
}

write.csv(scores, getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT", "scores_nmds_birds.csv"))

files <- list.files(getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT"), pattern = "veg_TAKE4.csv", full.names = T)

results <- lapply(files, read.csv) %>% 
  map(., select, model_var, conv, stress, permanova_veg_F, permanova_veg_R2, permanova_veg_p) %>% 
  do.call(rbind, .) %>% 
  filter(., conv == "TRUE") %>% 
  filter(., stress < 0.1) %>% 
  write.csv(getDataPath(chapter, "Fig1", "NMDS_BIRDS_OPT", "filtered_nmds_birds4.csv"), row.names = F)

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

#Plotly - insects all land variables

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
p_completeinsects <- layout(p_completeinsects, title = "Insects - all landscape variables (Stress:0.18)")
