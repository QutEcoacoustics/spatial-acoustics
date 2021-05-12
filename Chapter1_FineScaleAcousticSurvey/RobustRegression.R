#robust regression

library(MASS)
library(foreign)
library(olsrr)
library(robustbase)
library(tidyverse)
library(plm)

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

birds <- filter(summary_stats, class_model == "bird") %>% 
  droplevels(.)

cor <- cor(birds[c(70:77, 84, 85, 87:99)]) #%>% 
  #write.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "27.01.2021_birdcorrelation_landvariables.csv"))

ols <- lm(mean ~ CanopyCover +
            SubcanopyHeight +
            Slope +
            Aspect +
            DistWater +
            NDVI_AVERAGE +
            temperature +
            NT_HEIGHT_AVG +
            NS_DIST_AVG +
            GC_NG_AVG +
            GC_NF_AVG 
                  , data = birds)
summary(ols)

ols_plot_cooksd_bar(ols)
plot(ols)



lm.rob <- lmrob(mean ~ CanopyCover +
                  SubcanopyHeight +
                  Slope +
                  Aspect +
                  DistWater +
                  NDVI_AVERAGE +
                  temperature +
                  NT_HEIGHT_AVG +
                  NS_DIST_AVG +
                  GC_NG_AVG +
                  GC_NF_AVG , data = birds, method = "S")

summary(lm.rob)

lm.opt <- lmrob(mean ~CanopyCover +
                  Slope +
                  Aspect +
                  DistWater +
                  NDVI_AVERAGE +
                  temperature +
                  NT_HEIGHT_AVG
                  , data = birds, method = "S")

summary(lm.opt)


plot(lm.rob)





d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(birds, d1, r)

rabs <- abs(r)
a <- cbind (birds, d1, r, rabs)
asorted <- a[order(-rabs),]
asorted[1:10, ]

summary(rr.huber <- rlm(mean ~  NS_HEIGHT_AVG +
                          GC_NG_AVG +
                          GC_NF_AVG +
                          GC_BS_AVG +
                          CanopyCover +
                          SubcanopyHeight +
                          Slope +
                          Aspect +
                          Elevation +
                          NDVI_AVERAGE +
                          temperature, data = birds))
hweights <- data.frame(id = birds$id, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:40, ]

rrbisquare <- rlm(mean ~  NS_HEIGHT_AVG +
                    GC_NG_AVG +
                    GC_NF_AVG +
                    GC_BS_AVG +
                    CanopyCover +
                    SubcanopyHeight +
                    Slope +
                    Aspect +
                    Elevation +
                    NDVI_AVERAGE +
                    temperature, data = birds, psi = psi.bisquare)

summary(rrbisquare)
an <- anova(ols, test = "F")

biweights <- data.frame(id = birds$id, resid = rrbisquare$resid, weight = rrbisquare$w)
biweights2 <- biweights[order(rrbisquare$w), ]
biweights2[1:40, ]

summary(ols <- lm(mean ~ GC_NF_AVG + temperature, data = birds))
ols_plot_cooksd_bar(ols)

summary(rr.huber.opt <- rlm(mean ~  GC_NF_AVG +
                              temperature, data = birds))
hweights <- data.frame(id = birds$id, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:40, ]


summary(rrbisquare.opt <- rlm(mean ~  GC_NF_AVG +
                              temperature, data = birds, psi = psi.bisquare))
hweights <- data.frame(id = birds$id, resid = rrbisquare.opt$resid, weight = rrbisquare.opt$w)
hweights2 <- hweights[order(rrbisquare.opt$w), ]
hweights2[1:40, ]


plot(birds$mean, birds$GC_NF_AVG,
     xlab = "mean", ylab = "GC_NF_AVG", type = "p", 
     pch = 20, cex = .8)
abline(ols, col = 1) 
abline(rr.huber.opt, col = 2) 
abline(rrbisquare.opt, col = 3) 
legend(0, 80, c("OLS", "Huber", "Bisquare"),
       lty = rep(1, 7), bty = "n",
       col = c(1, 2, 3))


#Precision

training_rows <- sample(1:nrow(birds), 0.8 * nrow(birds))  
training_data <- birds[training_rows, ] 
test_data <- birds[-training_rows, ] 

lm_Predicted <- predict(ols, test_data)
rob_Predicted <- predict(rr.huber.opt, test_data)
bis_predicted <- predict(rrbisquare.opt, test_data)

lm_actuals_pred <- cbind(lm_Predicted, test_data$mean)
rob_actuals_pred <- cbind(rob_Predicted, test_data$mean)
bis_actuals_pred <- cbind(bis_predicted, test_data$mean)

mean(apply(lm_actuals_pred, 1, min)/
       apply(lm_actuals_pred, 1, max))

mean(apply(rob_actuals_pred, 1, min)/
       apply(rob_actuals_pred, 1, max))
  
mean(apply(bis_actuals_pred, 1, min)/
       apply(bis_actuals_pred, 1, max))
  
  
#Insects
  
insects <- filter(summary_stats, class_model == "insect") %>% 
  droplevels(.)

summary(ols <- lm(mean ~  CanopyCover +
                    SubcanopyHeight +
                    Slope +
                    Aspect +
                    Elevation +
                    DistWater +
                    NDVI_AVERAGE +
                    temperature +
                    NT_HEIGHT_AVG +
                    NS_DIST_AVG +
                    GC_NG_AVG +
                    GC_NF_AVG 
                    , data = insects))
plot(ols)


ols_plot_cooksd_bar(ols)

d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(insects, d1, r)

rabs <- abs(r)
a <- cbind (insects, d1, r, rabs)
asorted <- a[order(-rabs),]
asorted[1:10, ]

lm.rob <- lmrob(mean ~  CanopyCover +
                            SubcanopyHeight +
                            Slope +
                            Aspect +
                            Elevation +
                            DistWater +
                            NDVI_AVERAGE +
                            temperature +
                            NT_HEIGHT_AVG +
                            NS_DIST_AVG +
                            GC_NG_AVG +
                            GC_NF_AVG, data = insects, method = "S")
summary(lm.rob)

lm.opt <- lmrob(mean ~  CanopyCover +
                  Slope +
                  Aspect +
                  Elevation +
                  DistWater +
                  NDVI_AVERAGE +
                  NT_HEIGHT_AVG +

                  GC_NF_AVG, data = insects, method = "S")
summary(lm.opt)


hweights <- data.frame(id = birds$id, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:40, ]


rrbisquare <- rlm(mean ~  NS_HEIGHT_AVG +
                    GC_NG_AVG +
                    GC_NF_AVG +
                    GC_BS_AVG +
                    CanopyCover +
                    SubcanopyHeight +
                    Slope +
                    Aspect +
                    Elevation +
                    NDVI_AVERAGE +
                    temperature, data = insects, psi = psi.bisquare)

summary(rrbisquare)
an <- anova(ols, test = "F")

biweights <- data.frame(id = birds$id, resid = rrbisquare$resid, weight = rrbisquare$w)
biweights2 <- biweights[order(rrbisquare$w), ]
biweights2[1:40, ]

summary(rr.huber.opt <- rlm(mean ~  GC_NF_AVG +
                              temperature, data = birds))
hweights <- data.frame(id = birds$id, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:40, ]


