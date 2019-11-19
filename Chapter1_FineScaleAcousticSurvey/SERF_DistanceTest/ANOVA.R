library(tidyverse)
library(car)

AM10 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/SERF_DistanceTest/AM10_NOTUSEDFORTESTS/DistanceTest1_AM10.csv")
  as.integer(AM10$Distance..m.)

AM11 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/SERF_DistanceTest/AM11/DistanceTest1_AM11.csv")
AM12 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/SERF_DistanceTest/AM12/DistanceTest1_AM12.csv") %>% 
  select(., -"Obs")

AM13 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/SERF_DistanceTest/AM13/DistanceTest1_AM13.csv")
AM14 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/SERF_DistanceTest/AM14/DistanceTest1_AM14.csv")

total <- rbind(AM10, AM11, AM13, AM12, AM14)
TOTAL1 <- mutate(total, "Dist_factor" = as.factor(total$Distance..m.))

leveneTest(TOTAL1$Avg.Power..dB. ~ TOTAL1$Dist_factor)
leveneTest(TOTAL1$Peak.Power..dB. ~ TOTAL1$Dist_factor)
leveneTest(TOTAL1$Energy..dB. ~ TOTAL1$Dist_factor)

creek <- filter(TOTAL1, TOTAL1$Environment == "Creek")
boxplot(creek$Avg.Power..dB. ~ creek$Dist_factor)

creek.aov1 <- aov(creek$Avg.Power..dB. ~ creek$Dist_factor)
shapiro.test(creek.aov1$residuals)

kruskal.test(creek$Avg.Power..dB. ~ creek$Dist_factor)

boxplot(creek$Peak.Power..dB. ~ creek$Dist_factor)

creek.aov2 <- aov(creek$Peak.Power..dB. ~ creek$Dist_factor)
shapiro.test(creek.aov2$residuals)

kruskal.test(creek$Peak.Power..dB. ~ creek$Dist_factor)

creek.aov3 <- aov(creek$Energy..dB. ~ creek$Dist_factor)
shapiro.test(creek.aov3$residuals)

kruskal.test(creek$Energy..dB. ~ creek$Dist_factor)

Woodland <- filter(TOTAL1, total$Environment == "Eucalyptus")


Woodland.aov1 <- aov(Woodland$Avg.Power..dB. ~ Woodland$Dist_factor)
shapiro.test(Woodland.aov1$residuals)

kruskal.test(Woodland$Avg.Power..dB. ~ Woodland$Dist_factor)


Woodland.aov2 <- aov(Woodland$Peak.Power..dB. ~ Woodland$Dist_factor)
shapiro.test(Woodland.aov2$residuals)

kruskal.test(Woodland$Peak.Power..dB. ~ Woodland$Dist_factor)

Woodland.aov3 <- aov(Woodland$Energy..dB. ~ Woodland$Dist_factor)
shapiro.test(Woodland.aov3$residuals)

kruskal.test(Woodland$Energy..dB. ~ Woodland$Dist_factor)


Pasture <- filter(TOTAL1, total$Environment == "Pasture")


Pasture.aov1 <- aov(Pasture$Avg.Power..dB. ~ Pasture$Dist_factor)
shapiro.test(Pasture.aov1$residuals)

kruskal.test(Pasture$Avg.Power..dB. ~ Pasture$Dist_factor)


Pasture.aov2 <- aov(Pasture$Peak.Power..dB. ~ Pasture$Dist_factor)
shapiro.test(Pasture.aov2$residuals)

kruskal.test(Pasture$Peak.Power..dB. ~ Pasture$Dist_factor)

Pasture.aov3 <- aov(Pasture$Energy..dB. ~ Pasture$Dist_factor)
shapiro.test(Pasture.aov3$residuals)

kruskal.test(Pasture$Energy..dB. ~ Pasture$Dist_factor)
