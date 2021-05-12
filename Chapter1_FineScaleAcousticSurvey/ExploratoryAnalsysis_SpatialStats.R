#Spatial Stats on time-series
library(tidyverse)
library(purrr)
library(sp)
library(ggplot2)
library(ape)
library(lme4)
library(MuMIn)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
  
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

indices <- read.csv(getDataPath(chapter, "27.01.2021_LabelsIndicesLand.csv"))

summary_stats <- group_by(indices, id) %>% 
  summarise(., mean = mean(index_value), sd = sd(index_value)) %>% 
  left_join(indices, ., by = "id") %>% 
  distinct(., id, .keep_all = T)

summary_stats$LAT <- signif(summary_stats$LONG, 7)

summary_stats <- summary_stats %>% mutate_at(c(70:77, 84, 85, 87:99), scale)

summary(summary_stats)

counts <- group_by(summary_stats, LONG) %>% 
  count(class_model)
  
ggplot(counts, aes(x = as.factor(LONG))) +
    geom_bar(stat = "identity", aes(y = n)) +
    theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = "Point") +
  ggsave(getDataPath(chapter, "Figures", "21.01.2021_motifsperpoint.jpg"))

ggplot(counts, aes(x = as.factor(LONG))) +
  geom_bar(stat = "identity", aes(y = n, fill = class_model), position = "dodge") +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = "Point") +
  ggsave(getDataPath(chapter, "Figures", "21.01.2021_motifsperpoint_labels.jpg"))


birds <- filter(summary_stats, class_model == "bird") %>% 
  droplevels(.)

cor <- cor(birds[c(70:77, 84, 85, 87:97)], method = "spearman") %>% 
  write.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "11.02.2021_birdcorrelation_landvariables.csv"))

summary(birds)


bird_lmer <- lm(formula = mean ~ CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + NDVI_AVERAGE + temperature + NT_DIST_AVG + NS_DIST_AVG + GC_NG_AVG + GC_NF_AVG + GC_SH_AVG, data = birds)
summary(bird_lmer)

bird_lm.opt <- lm(formula = mean ~ CanopyHeight + SubcanopyHeight + Slope + NDVI_AVERAGE + temperature + NT_DIST_AVG + NS_DIST_AVG + GC_NG_AVG + GC_NF_AVG + GC_SH_AVG * point , data = birds)
summary(bird_lm.opt)

bird_lmer <- lmer(formula = sd ~ CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + NDVI_AVERAGE + temperature + NT_DIST_AVG + NS_DIST_AVG + GC_NG_AVG + GC_NF_AVG + GC_SH_AVG + (1 | point) , data = birds)
r.squaredGLMM(birds.model)
summary(birds.model)


null <- glm(mean~1, data = birds)

anova(birds.model, null, test = "F")

anova(birds.model, test = "F")

bird_glm <- glm(formula = mean ~ CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + NDVI_AVERAGE + temperature + NT_DIST_AVG + NS_DIST_AVG + GC_NG_AVG + GC_NF_AVG + GC_SH_AVG, data = birds)

summary(bird_glm)
r.squaredGLMM(bird_glm)

lm.opt <- glm(mean ~ SubcanopyHeight + temperature + NT_DIST_AVG + GC_NG_AVG + GC_NF_AVG , data = birds)

summary(lm.opt)
r.squaredGLMM(lm.opt)

anova(bird_model, lm.opt, test = "F")
anova(lm.opt, null, test = "F")

anova(lm.opt, test = "F")
r.squaredGLMM(lm.opt)

bird.dists <- as.matrix(dist(cbind(birds$LONG, birds$LAT)))

birds.dists.inv <- 1/bird.dists
diag(birds.dists.inv) <- 0

birds.dists.inv[is.infinite((birds.dists.inv))] <- 0

Moran.I(lm.opt$residuals, birds.dists.inv)

summary(lm.opt)

lm.opt <- lm(mean ~ GC_NF_AVG + temperature, data = birds)
summary(lm.opt)


#Insects

insects <- filter(summary_stats, class_model == "insect") %>% 
  droplevels(.)

cor <- cor(insects[c(29,34,43,48,53,58,63,68,73, 75:81,88,89,91,92)], method = "spearman") %>% 
  write.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "21.01.2021_insectcorrelation_landvariables.csv"))


insect_model <- lmer(mean ~ NS_DIST_AVG +
                  GC_NG_AVG +
                  GC_NF_AVG +
                  GC_LT_AVG +
                  GC_SH_AVG +
                  CanopyCover +
                  ShrubCover +
                  CanopyHeight +
                  Slope +
                  Aspect +
                  Elevation +
                  NDVI_AVERAGE +
                  temperature + (1|point), data = insects)
summary(insect_model)
r.squaredGLMM(insect_model)

null <- glm(mean~1, data = insects)

anova(insect_model, null, test = "F")

anova(insect_model, test = "F")

lm.opt <- lmer(mean ~ NS_DIST_AVG +
                 GC_NF_AVG +
                 GC_LT_AVG +
                 CanopyCover +
                 ShrubCover +
                 CanopyHeight +
                 Slope +
                 Aspect +
                 temperature + (1|point), data = insects)
r.squaredGLMM(lm.opt)
summary(lm.opt)

anova(insect_model, lm.opt, test = "F")
anova(lm.opt, null, test = "F")

anova(lm.opt, test = "F")

summary(lm.opt)
r.squaredGLMM(lm.opt)

insect.dists <- as.matrix(dist(cbind(insects$LONG, insects$LAT)))

insect.dists.inv <- 1/insect.dists
diag(insect.dists.inv) <- 0

insect.dists.inv[is.infinite((insect.dists.inv))] <- 0

Moran.I(lm.opt$residuals, insect.dists.inv)

summary(lm.opt)

29.025/132


29.025/35.059


#Multiple R-squared: model performance; Coefficients: each explanatory variable; F stats and p value = model significanceAdjusted R sqaured takes into account model complexity (number of variables) - more accurate measure of model performanceThe coefficient for each explanatory variable reflects both the strength and type of relationship the explanatory variable has to the dependent variable. When the sign associated with the coefficient is negative, the relationship is negative (for example, the larger the distance from the urban core, the smaller the number of residential burglaries). When the sign is positive, the relationship is positive (for example, the larger the population, the larger the number of residential burglaries). Coefficients are given in the same units as their associated explanatory variables (a coefficient of 0.005 associated with a variable representing population counts may be interpreted as 0.005 people). The coefficient reflects the expected change in the dependent variable for every 1-unit change in the associated explanatory variable, holding all other variables constant (for example, a 0.005 increase in residential burglary is expected for each additional person in the census block, holding all other explanatory variables constant). The T test is used to assess whether an explanatory variable is statistically significant. The null hypothesis is that the coefficient is, for all intents and purposes, equal to zero (and consequently is not helping the model). When the probability or robust probability (p-value) is very small, the chance of the coefficient being essentially zero is also small. An explanatory variable associated with a statistically significant coefficient is important to the regression model if theory or common sense supports a valid relationship with the dependent variable if the relationship being modeled is primarily linear, and if the variable is not redundant to any other explanatory variables in the model.#

#Mean differences

#H0: data are normally distributed; if p < 0.05 = reject null - data not normally ditributed

shapiro.test(birds$mean)

shapiro.test(insects$mean)

shapiro.test(summary_stats$mean)

#Everybody not normally distributed

#Kruskal wallis between means: birds vs insects; different points


kruskal.test(summary_stats$mean, summary_stats$class_model)
w <- wilcox.test(insects$mean, birds$mean)
summary(w)

ggplot(summary_stats, aes(class_model, mean)) +
  geom_boxplot() +
  theme_classic() +
  ggsave(getDataPath(chapter, "Figures", "25.01.2021_groupmeans.jpg"))

kruskal.test(summary_stats$mean, summary_stats$point)
dunn.test::dunn.test(summary_stats$mean, summary_stats$point)

ggplot(summary_stats, aes(as.factor(LONG), mean)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = "Point") +
  ggsave(getDataPath(chapter, "Figures", "25.02.2021_groupmeansperpoint.jpg"))

kruskal.test(insects$mean, insects$point)
dunn.test::dunn.test(insects$mean, insects$point)

ggplot(birds, aes(as.factor(LONG), mean*10)) +
  geom_boxplot() +
  geom_point(aes(as.factor(LONG), GC_NF_AVG, colour = "red", size = 4)) +
  geom_boxplot(aes(as.factor(LONG), temperature, colour = "red")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 10), name = "Mean Index Values")+
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = "Point") +
  ggsave(getDataPath(chapter, "Figures", "25.02.2021_birdmeansperpoint.jpg"))
