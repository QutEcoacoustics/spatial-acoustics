

rm(list = ls())

library(tidyverse)
library(ggplot2)
# library(stringi)
# library(car)
# library(data.table)
library(MuMIn)
library(vegan)
library(plotly)
library(processx)
library(lme4)
library(coefplot)
library(merTools)
library(sjPlot)
library(sjmisc)
library(report)
library(glmmTMB)

#Functions ----
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

my_nmds <- function(dataframe) {
  
  result <<- NULL

  dataframe_norm <<- mutate_at(dataframe, c(7:ncol(dataframe)), decostand, method = "range")
  rownames(dataframe_norm) <<- dataframe_norm$X
  
  nmds <<- metaMDS(dataframe_norm[,7:ncol(dataframe)], k = 2, trymax = 100)

  data.scores <<- as.data.frame(scores(nmds))
  
  data.scores$id <<- rownames(data.scores)
  data.scores$site <<- dataframe$site
  data.scores$species <<- dataframe$RFclass
  
  species.scores <<- as.data.frame(scores(nmds, "species"))
  species.scores$var <<- rownames(species.scores)
  
  p <<- plot_ly()
  p <<- add_trace(p, name = data.scores$site, type = "scatter", x = data.scores$NMDS1, y = data.scores$NMDS2, text = data.scores$site, symbol =  data.scores$species, symbols = c("bird" = "x",
"frog" = "square",
"insect" = "+"), 
                  fillcolor = data.scores$site)
  p <<- add_trace(p, name = "Environmental variables", type = "scatter", mode = "text", x = species.scores$NMDS1, y = species.scores$NMDS2, text = species.scores$var)
  p <<- add_trace(p, name = data.scores$species, type = "scatter", mode = "markers", x = data.scores$NMDS1, y = data.scores$NMDS2, symbol = data.scores$species, opacity = 0)
  p
  
  PERMANOVA <<- adonis(dataframe[,c(4:ncol(dataframe))]~dataframe$site) 
  
  result$conv <<- as.character(nmds$converged)
  result$stress <<- as.numeric(nmds$stress)
  result$permanova_F <<- as.numeric(PERMANOVA$aov.tab$F.Model[1])
  result$permanova_R2 <<- as.numeric(PERMANOVA$aov.tab$R2[1])
  result$permanova_p <<- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
  result <<- as.data.frame(result)
  
}

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95, .drop = TRUE) {
  
  library(plyr)
  
  #New version of length which can handle NA'S if na.rm ==T, don't count them
  length2 <- function(x, na.rm = F) {
    if(na.rm) sum(!is.na(x))
    else length(x)
  }
  
  
  #This does the summary. For each group's dataframe, return a vector with N, mean and sd
  datac <- ddply(data, groupvars, .drop = .drop, .fun = function(xx, col) {
    c(N = length2(xx[[col]], na.rm = na.rm),
      mean = mean(xx[[col]], na.rm = na.rm),
      sd = sd(xx[[col]], na.rm = na.rm)
      ) },
    measurevar
  )
  
  
  #Rename the "mean" column
  datac <- mutate(datac, measure_var = measurevar)
  #datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd/sqrt(datac$N) #calculate sd of the mean
  
  #Confidence interval multiplier for sd
  #calculate t-stat for CI:
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#Reading and preparing the data----
complete_df <- read.csv(getDataPath("13.05.2022_fixingdata5.csv"))

# new_df <- complete_df %>% rowwise() %>% 
# mutate(., mean_temp = mean(c(temp_max,temp_min)))
# 
# write.csv(complete_df, getDataPath("13.05.2022_fixingdata4.csv"))


#1. NMDS ----
#One for large scale, one for small scale and one with both
#A. Large scale environmental variables ----
#One for SPLA and one for SPLC
large_var <- dplyr::select(new_df, site, RFclass, soil_3k, urban_3k, water_3k, cleared_3k, natural_cover_3k, contag_landscape_3k, np_landscape_3k, tca_landscape_3k, period, bvg, month) %>% na.exclude()
small_var <- dplyr::select(new_df, site, RFclass, period, bvg, month, soil_325, urban_325, water_325, cleared_325, natural_cover_325, contag_landscape_325, np_landscape_325, tca_landscape_325,  ndvi_mean, ebi_max, ndwi_mean, rain_value, mean_temp) %>% na.exclude()

#Removing highly correlated variables
#Threshold of 0.6 used
cor <- cor(large_var[,3:10]) %>% 
write.csv(getDataPath("correlation_NMDS_largeA.csv"))

#Intensive use and conservation were neg correlated and I left conservation just because of the C results; Production from irrigated and production from natural were hiiighly positively correlated so I left production from irrigated

#Final df for SPLA
large_var <- dplyr::select(new_df, X, site, RFclass, period, bvg, month, soil_3k, cleared_3k, natural_cover_3k, np_landscape_3k) %>%
  na.exclude() %>% 
  droplevels()

PERMANOVA <- adonis2(large_var[,c(6:ncol(large_var))]~large_var$RFclass) 
result <- NULL

result$permanova_F <- as.numeric(PERMANOVA$aov.tab$F.Model[1])
result$permanova_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
result$permanova_p <- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
result <<- as.data.frame(result)
write.csv(result, getDataPath("permanova_newhybrid_large.csv"))

cor <- cor(small_var[,6:18]) %>% 
  write.csv(getDataPath("correlation_NMDS_largeC.csv"))

#Production from irrigated and production from natural were hiiighly positively correlated so I left production from irrigated; Water and intensive use were also highly correlated here. I will then remove intensive and leave conservation and water

#Final for SPLC
small_var <- dplyr::select(new_df, X, site, RFclass, period, bvg, month, soil_325, urban_325, cleared_325, natural_cover_325, contag_landscape_325, tca_landscape_325,  ndvi_mean, rain_value, mean_temp) %>%
  na.exclude() %>% 
  droplevels()

#SPLA ----
set.seed(123)

my_nmds(large_var)

p

write.csv(result, getDataPath("large_varA.csv"))

#SPLC ----


PERMANOVA <- adonis2(small_var[,c(7:ncol(small_var))]~small_var$RFclass) 
result <- NULL

result$permanova_F <- as.numeric(PERMANOVA$aov.tab$F.Model[1])
result$permanova_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
result$permanova_p <- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
result <<- as.data.frame(result)
write.csv(result, getDataPath("permanova_newhybrid_large.csv"))

my_nmds(small_var)

p

write.csv(result, getDataPath("large_varC.csv"))

#B. Small scale environmental variables ----
#One for SPLA and one for SPLC
small_varA <- dplyr::select(complete_df, Site, ID, SPL_A, Hor_pos_water_cms, Ver_pos_water_cms, NearestRoad, bare, CWD, grass, litter, rock, roots, sand, tall.grass, trunk, water) %>% na.exclude()
small_varC <- select(complete_df, Site, ID, SPL_C, Hor_pos_water_cms, Ver_pos_water_cms, NearestRoad, bare, CWD, grass, litter, rock, roots, sand, tall.grass, trunk, water) %>% na.exclude()

#Removing highly correlated variables -
#Threshold of 0.6 used
cor <- cor(small_varA[,4:14]) %>% 
  write.csv(getDataPath("correlation_NMDS_smallA.csv"))

#Excluded: rock (correlated with SPLA and neg to litter) and CWD (correlated with tall grass)

#Final df for SPLA
small_varA <- dplyr::select(complete_df, Site, ID, NewHybrid, SPL_A, Hor_pos_water_cms, Ver_pos_water_cms, bare, grass, litter, roots, sand, tall.grass, trunk, water) %>%
  na.exclude() %>% 
  droplevels()

PERMANOVA <- adonis(small_varA[,c(4:ncol(small_varA))]~small_varA$NewHybrid) 
result <- NULL

result$permanova_F <- as.numeric(PERMANOVA$aov.tab$F.Model[1])
result$permanova_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
result$permanova_p <- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
result <<- as.data.frame(result)
write.csv(result, getDataPath("permanova_newhybrid.csv"))


cor <- cor(small_varC[,3:12]) %>% 
  write.csv(getDataPath("correlation_NMDS_smallC.csv"))

#same as for A to exclude

#Final for SPLC
small_varC <- dplyr::select(complete_df, Site, ID, NewHybrid, SPL_C, Hor_pos_water_cms, Ver_pos_water_cms, bare, grass, litter, roots, sand, tall.grass, trunk, water) %>%
  na.exclude() %>% 
  droplevels()

#SPLA ----
set.seed(123)

my_nmds(small_varA)
p
write.csv(result, getDataPath("small_varA.csv"))

#SPLC ----

my_nmds(small_varC)
p
write.csv(result, getDataPath("small_varC.csv"))


#C. Both small and large environmental var ----
#One for SPLA and one for SPLC
both_varA <- select(complete_df, Site, ID, SPL_A, IntensiveUse, HighwaysAndSecondary, NearestRoad, Conservation.and.natural.environments, Production.from.irrigated.agriculture.and.plantations, Production.from.relatively.natural.environments, Water, Hor_pos_water_cms, Ver_pos_water_cms, NearestRoad, bare, CWD, grass, litter, rock, roots, sand, tall.grass, trunk, water) %>% na.exclude()
both_varC <- select(complete_df, Site, ID, SPL_C, IntensiveUse, HighwaysAndSecondary, NearestRoad, Conservation.and.natural.environments, Production.from.irrigated.agriculture.and.plantations, Production.from.relatively.natural.environments, Water, Hor_pos_water_cms, Ver_pos_water_cms, NearestRoad, bare, CWD, grass, litter, rock, roots, sand, tall.grass, trunk, water) %>% na.exclude()

#Removing highly correlated variables -
#Threshold of 0.6 used
cor <- cor(both_varA[,3:22]) %>% 
  write.csv(getDataPath("correlation_NMDS_bothA.csv"))

#Excluded: rock, intensive use, production from natural, Highways and secondary, CWD (correlated with tall grass)

#Final df for SPLA
both_varA <- select(complete_df, Site, ID, SPL_A, NearestRoad, Conservation.and.natural.environments, Production.from.irrigated.agriculture.and.plantations, Water, Hor_pos_water_cms, Ver_pos_water_cms, NearestRoad, bare, grass, litter, roots, sand, tall.grass, trunk, water) %>%
  na.exclude() %>% 
  droplevels()

cor <- cor(both_varC[,3:22]) %>% 
  write.csv(getDataPath("correlation_NMDS_bothC.csv"))

#Excluded: rock, intensive use, production from natural, CWD (correlated with tall grass)

#Final for SPLC
both_varC <- select(complete_df, Site, ID, SPL_C, HighwaysAndSecondary, NearestRoad, Conservation.and.natural.environments, Production.from.irrigated.agriculture.and.plantations, Water, Hor_pos_water_cms, Ver_pos_water_cms, NearestRoad, bare, grass, litter, roots, sand, tall.grass, trunk, water) %>%
  na.exclude() %>% 
  droplevels()

#SPLA ----

my_nmds(both_varA)
p
write.csv(result, getDataPath("both_varA.csv"))

#SPLC ----

my_nmds(both_varC)
p
write.csv(result, getDataPath("both_varC.csv"))

#D. Call traits + small scale variables ----

#One for SPLA and one for SPLC
call_varA <- select(complete_df, Site, ID, SPL_A, Peak.Freq.Temp.Adj, Call.Dur, Note.Dur.Weight.Adj, Internote.Int.Temp.Adj, MeanPulseDur.Weight.Adj, MeanPulseRate.Temp.Adj, MeanInterPulse.Temp.Adj, Hor_pos_water_cms, Ver_pos_water_cms, NearestRoad, bare, CWD, grass, litter, rock, roots, sand, tall.grass, trunk, water) %>% na.exclude()
call_varC <- select(complete_df, Site, ID, SPL_C, Peak.Freq.Temp.Adj, Call.Dur, Note.Dur.Weight.Adj, Internote.Int.Temp.Adj, MeanPulseDur.Weight.Adj, MeanPulseRate.Temp.Adj, MeanInterPulse.Temp.Adj, Hor_pos_water_cms, Ver_pos_water_cms, NearestRoad, bare, CWD, grass, litter, rock, roots, sand, tall.grass, trunk, water) %>% na.exclude()

#ATTENTION: DID NOT EXCLUDE ANY VARIABLES BECAUSE OF THE CALL ONES ----

my_nmds(both_varA)
p
write.csv(result, getDataPath("call_varA.csv"))

#Final for SPLC

my_nmds(call_varC)
p
write.csv(result, getDataPath("call_varC.csv"))


#GLMM ----
group_vars <- c("NewHybrid", "Site")

#SPLA----
small_varA <- dplyr::select(complete_df, Site, ID, NewHybrid, SPL_A, Hor_pos_water_cms, Ver_pos_water_cms, water, litter, rock) %>%
  na.exclude() %>% 
  droplevels()

#Call.Dur: Best model for call duration: all the variables below + species + interactions; r2m = 0.3957; r2c = 0.6537----

call_trait <- "Call.Dur"

df <- dplyr::select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varA) %>% 
  dplyr::select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")

model <- lme4::lmer(Call.Dur ~ SPL_A * Hor_pos_water_cms * Ver_pos_water_cms * water  + (1|NewHybrid) + (1|Site), data = df_norm)

model <- lmer(Call.Dur ~ SPL_A + Hor_pos_water_cms + Ver_pos_water_cms + Hor_pos_water_cms:SPL_A + SPL_A:Ver_pos_water_cms + (1|Site) + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)
fixef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <<- model.avg(get.models(modeltable, subset = T))
summary(mod2)

summarise_calldur <- summarySE(df_norm, measurevar = "Call.Dur", groupvars = group_vars)
summarise_water <- summarySE(df_norm, measurevar = "water", groupvars = group_vars)
summarise_SPLA <- summarySE(df_norm, measurevar = "SPL_A", groupvars = group_vars)
summarise_hor <- summarySE(df_norm, measurevar = "Hor_pos_water_cms", groupvars = group_vars)
summarise_ver <- summarySE(df_norm, measurevar = "Ver_pos_water_cms", groupvars = group_vars)

grouped <- select(complete, NewHybrid, measure_var, mean, ci) %>% 
  group_by(NewHybrid, measure_var)

ggplot(data = summarise_calltrait, aes(x = Site, y = Call.Dur, fill = NewHybrid)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=Call.Dur-ci, ymax = Call.Dur+ci),
                width = .2,
                position = position_dodge(.9)) +
  geom_bar(data = summarise_SPLA, aes(x = Site, y = SPL_A, fill = NewHybrid)) +
    geom_errorbar(aes(ymin=SPL_A-ci, ymax = SPL_A+ci),
                  width = .2,
                  position = position_dodge(.9))
confint.merMod(model, level = 0.95, method = "boot", oldNames = F)
REsim(model)


plot <- plot_model(model, type = "pred", terms = c("Hor_pos_water_cms", "Site"))
plot
plot$SPL_A
plot$Hor_pos_water_cms
plot$Ver_pos_water_cms

plot <- plot_model(model, type = "int")
plot[[1]]
plot[[2]]


#Note.Dur.Weight.Adj look into it----

call_trait <- "Note.Dur.Weight.Adj"

df <- dplyr::select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varA) %>% 
  dplyr::select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lme4::lmer(Note.Dur.Weight.Adj ~ SPL_A * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid) + (1|Site), data = df_norm)

model <- lm(Note.Dur.Weight.Adj ~ SPL_A + Hor_pos_water_cms + Ver_pos_water_cms + water + Hor_pos_water_cms:Ver_pos_water_cms + Hor_pos_water_cms:water + SPL_A:water, data = df_norm)
df_norm$SPL_A <- as.numeric(df_norm$SPL_A)
df_norm$Hor_pos_water_cms <- as.numeric(df_norm$Hor_pos_water_cms)
df_norm$water <- as.numeric(df_norm$water)
df_norm$Ver_pos_water_cms <- as.numeric(df_norm$Ver_pos_water_cms)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

summarise_notedur <- summarySE(df_norm, measurevar = call_trait, groupvars = group_vars)
summarise_water <- summarySE(df_norm, measurevar = "water", groupvars = group_vars)
summarise_SPLA <- summarySE(df_norm, measurevar = "SPL_A", groupvars = group_vars)
summarise_hor <- summarySE(df_norm, measurevar = "Hor_pos_water_cms", groupvars = group_vars)
summarise_hor <- summarySE(df_norm, measurevar = "Ver_pos_water_cms", groupvars = group_vars)

complete <- rbind(summarise_calltrait, summarise_water, summarise_SPLA, summarise_hor)

grouped <- select(complete, NewHybrid, measure_var, mean, ci) %>% 
  group_by(NewHybrid, measure_var)
report(model)


plot <- plot_model(model, type = "int")

#Note.Rate.Weight.Adj complete model and loots of influence of site----

call_trait <- "Note.Rate.Weight.Adj"

df <- dplyr::select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varA) %>% 
  dplyr::select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lme4::lmer(Note.Rate.Weight.Adj ~ SPL_A * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid) + (1|Site), data = df_norm)

model <- lme4::lmer(Note.Rate.Weight.Adj ~ SPL_A + (1|Site), data = df_norm)

summary(model)
report(model)
ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

summarise_noterate <- summarySE(df_norm, measurevar = call_trait, groupvars = group_vars)

noterate <- rbind(enviro, summarise_noterate)

plot_model(model, type = "pred")


#MeanPulseDur.Weight.Adj - look into best model is the complete with lots of influence of the species----
call_trait <- "MeanPulseDur.Weight.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varA) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")
df_norm$SPL_A <- as.numeric(df_norm$SPL_A)
df_norm$Hor_pos_water_cms <- as.numeric(df_norm$Hor_pos_water_cms)
df_norm$water <- as.numeric(df_norm$water)

model <- lmer(MeanPulseDur.Weight.Adj ~ SPL_A * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid) + (1|Site), data = df_norm)

model <- lm(MeanPulseDur.Weight.Adj ~ SPL_A + Hor_pos_water_cms + water + SPL_A:Hor_pos_water_cms + Hor_pos_water_cms:water + SPL_A:water + SPL_A:Hor_pos_water_cms:water, data = df_norm)

report(model)
summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

summarise_pulsedur <- summarySE(df_norm, measurevar = call_trait, groupvars = group_vars)

calldur_model_summary <- rbind(summarise_calldur, summarise_hor, summarise_SPLA, summarise_ver)

calldur_model <- filter(calldur_model_summary, measure_var == "Call.Dur") %>% 
  droplevels()
enviro <- filter(calldur_model_summary, measure_var != "Call.Dur") %>% 
  droplevels()

ggplot(data = df_norm, aes(x = Hor_pos_water_cms, y = MeanPulseDur.Weight.Adj, size = water, colour = SPL_A)) +
  geom_jitter()+
  scale_color_gradient(low = "blue", high = "red") +
  xlab("Horizontal position") + ylab("Mean Pulse Duration") +
  labs(colour = "SPL A", size = "Water") +
  theme_bw()

ggplot(data = df_norm, aes(x = Hor_pos_water_cms, y = MeanPulseDur.Weight.Adj, size =  water)) +
  geom_jitter() +
  xlab("Horizontal position") + ylab("Mean Pulse Duration") +
  labs(size = "Water")

with(df_norm, {
interaction.plot(Site, NewHybrid, c("Hor_pos_water_cms"), fun = mean, type = "l") })

plot_model(model, type = "int")

#MeanPulseRate.Temp.Adj null model is best----
call_trait <- "MeanPulseRate.Temp.Adj"

df <- dplyr::select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varA) %>% 
  dplyr::select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lme4::lmer(MeanPulseRate.Temp.Adj ~ SPL_A * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid) + (1|Site), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)



#MeanInterPulse.Temp.Adj null model is best----
call_trait <- "MeanInterPulse.Temp.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varA) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lmer(MeanInterPulse.Temp.Adj ~ SPL_A * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid) + (1|Site), data = df_norm)


summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Second model - best model is the one below - only the interaction between SPLA and SPLA are significant but removing the other variables makes the r2 much worse. Still the model performs better than the null

model <- lm(MeanInterPulse.Temp.Adj ~ SPL_A * Ver_pos_water_cms * water, data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Peak.Freq.Temp.Adj null model is better----

call_trait <- "Peak.Freq.Temp.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude() %>% 
  droplevels() %>%  
  merge(., small_varA) %>% 
  select(ID, Site, NewHybrid, everything())

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")
cor(df[,5:ncol(df)])

model_FreqTempAdjusA <- lmer(Peak.Freq.Temp.Adj ~  SPL_A * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid) + (1|Site), data = df_norm)

model_FreqTempAdjusA <- lm(Peak.Freq.Temp.Adj ~  SPL_A + Ver_pos_water_cms, data = df_norm)

summary(model_FreqTempAdjusA)

ranef(model_FreqTempAdjusA)

plot(model_FreqTempAdjusA)

coefplot(model_FreqTempAdjusA)

r.squaredGLMM(model_FreqTempAdjusA)
options(na.action = na.fail)
modeltable <- dredge(model_FreqTempAdjusA)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)



call_trait <- "Call.Dur"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varA) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")



#Internote.Int.Temp.Adj best model is null----

call_trait <- "Internote.Int.Temp.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varA) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lmer(Internote.Int.Temp.Adj ~ SPL_A * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid) + (1|Site), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)


#SPLC ----
small_varC <- select(complete_df, Site, ID, NewHybrid, SPL_C, Hor_pos_water_cms, Ver_pos_water_cms, litter, water, rock) %>%
  na.exclude() %>% 
  droplevels()
#Peak.Freq.Temp.Adj Null performs better and no influence of species----

call_trait <- "Peak.Freq.Temp.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude() %>% 
  droplevels() %>%  
  merge(., small_varC) %>% 
  select(ID, Site, NewHybrid, everything())

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")
cor(df[,5:ncol(df)])

model_FreqTempAdjusA <- lmer(Peak.Freq.Temp.Adj ~  SPL_C * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid), data = df_norm)

summary(model_FreqTempAdjusA)

ranef(model_FreqTempAdjusA)

plot(model_FreqTempAdjusA)

coefplot(model_FreqTempAdjusA)

r.squaredGLMM(model_FreqTempAdjusA)
options(na.action = na.fail)
modeltable <- dredge(model_FreqTempAdjusA)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Second best model is just with SPL_C and it has r2 of 20

model_FreqTempAdjusA <- lm(Peak.Freq.Temp.Adj ~  SPL_C, data = df_norm)

summary(model_FreqTempAdjusA)

ranef(model_FreqTempAdjusA)

plot(model_FreqTempAdjusA)

coefplot(model_FreqTempAdjusA)

r.squaredGLMM(model_FreqTempAdjusA)
options(na.action = na.fail)
modeltable <- dredge(model_FreqTempAdjusA)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)
#Call.Dur look into it----

call_trait <- "Call.Dur"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varC) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <<- lmer(Call.Dur ~ SPL_C * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <<- dredge(model)
mod2 <<- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Second model - is the best with 57 exaplanation and lots of influence of sp - interesting that jungguy allopatric is the biggest influence, followed by wilcoxii in hybrid and seems like the hybrids have almost no effect at all ----

model <<- lmer(Call.Dur ~ SPL_C * Hor_pos_water_cms + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <<- dredge(model)
mod2 <<- model.avg(get.models(modeltable, subset = T))
summary(mod2)

summarise_calltrait <- summarySE(df_norm, measurevar = "SPL_C", groupvars = group_vars)

complete <- rbind(summarise_calltrait, complete)

#Note.Dur.Weight.Adj look into it----

call_trait <- "Note.Dur.Weight.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varC) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lmer(Note.Dur.Weight.Adj ~ SPL_C * Hor_pos_water_cms * Ver_pos_water_cms* water + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#SecondModel - interaction btw the 3 explains 51% of the model and the newhybrid explains the other 7%. For this one seems like the hybrids and wilcoxii in hybrid are the most important of random.

model <- lmer(Note.Dur.Weight.Adj ~ SPL_C * Hor_pos_water_cms * water + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Note.Rate.Weight.Adj null model performs best - don't use it----

call_trait <- "Note.Rate.Weight.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varC) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lmer(Note.Rate.Weight.Adj ~ SPL_C * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)


#Internote.Int.Temp.Adj look into it----

call_trait <- "Internote.Int.Temp.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varC) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lmer(Internote.Int.Temp.Adj ~ SPL_C * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Second model seems to be better - no influence of species; interaction between all variables and SPLC are the significant variables with r2 of 0.41

model <- lm(Internote.Int.Temp.Adj ~ SPL_C * Hor_pos_water_cms  * water, data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#MeanPulseDur.Weight.Adj - null model is performing better----
call_trait <- "MeanPulseDur.Weight.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varC) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lmer(MeanPulseDur.Weight.Adj ~ SPL_C * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#MeanPulseRate.Temp.Adj this model seems to have the majority of explanation by the species. The null model always perfomerd best than the fixed variables but still getting an r2 of approx 32 with one the random effect - shouldn't use the model but maybe discussion/considerations in the final discussion----
call_trait <- "MeanPulseRate.Temp.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varC) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lmer(MeanPulseRate.Temp.Adj ~ SPL_C * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Second model - SPLC alone performs better than null - no influence of sp and r2 of 0.2
model <- lm(MeanPulseRate.Temp.Adj ~ SPL_C, data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#MeanInterPulse.Temp.Adj SPLC alone + influence of species explain mean pulse inter rate with an rmarginal of 0.18 and an conditional r of 0.22◘----
call_trait <- "MeanInterPulse.Temp.Adj"

df <- select(complete_df, all_of(call_trait), ID) %>%
  na.exclude()  %>%  
  merge(., small_varC) %>% 
  select(ID, Site, NewHybrid, everything() )%>% 
  droplevels()

df_norm <- mutate_at(df, c(4:ncol(df)), decostand, method = "range")


model <- lmer(MeanInterPulse.Temp.Adj ~ SPL_C * Hor_pos_water_cms * Ver_pos_water_cms * water + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Second model - SPLC alone + influence of species explain mean pulse inter rate with an rmarginal of 0.18 and an conditional r of 0.22

model <- lmer(MeanInterPulse.Temp.Adj ~ SPL_C + (1|NewHybrid), data = df_norm)

summary(model)

ranef(model)

plot(model)

coefplot(model)

r.squaredGLMM(model)
options(na.action = na.fail)
modeltable <- dredge(model)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

