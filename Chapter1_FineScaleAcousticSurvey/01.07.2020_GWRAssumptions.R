library(tidyverse)
library(caTools)
library(lmtest)
library(ape)

rm(list = ls())

#Pre GWR - testing assumptions and preparing data

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

df <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv"))

df <- df %>% mutate_at(c(73:80, 87:101), scale)

#Splitting data into training and testing
data_split <- sample.split(df, SplitRatio = 0.75)
train <- subset(df, data_split == TRUE)
test <- subset(df, data_split == FALSE)

#Splitting into bird and insect df
bird_df <- filter(df, class_model == "bird") %>% 
  droplevels(.)

#Correlation between variables
# cor <- cor(bird_df[c(73:80, 87:101)]) %>% 
#   write.csv(getDataPath(chapter, "28.02.2021_birdcorrelation_landvariables.csv"))

insect_df <- filter(df, class_model == "insect") %>% 
  droplevels(.)

# cor <- cor(insect_df[c(73:80, 87:101)]) %>% 
#   write.csv(getDataPath(chapter, "28.02.2021_insectcorrelation_landvariables.csv"))

#same variables were correlated and therefore removed so I an just change the DF and keep variables :)

#Linear Model 
global_model <- lm(mean ~ CanopyCover +
                   ShrubCover +
                   CanopyHeight +
                   SubcanopyHeight +
                   Slope +
                   Aspect +
                   Elevation +
                   DistWater +
                   aug_ndvi_avg +
                   mean_temp +
                   sd_temp +
                   sd_humi +
                   NT_DIST_AVG +
                   NS_DIST_AVG +
                   GC_NG_AVG +
                   GC_NF_AVG +
                   GC_BS_AVG +
                   GC_SH_AVG, data = insect_df)

summary(global_model)

#Heterocedasticidade test - evaluates if there is non constant variance in the model errors; p value significant means that the relationship between explanatory and response variables are non stationary
lm.bp <- bptest(mean ~ CanopyCover +
                  ShrubCover +
                  CanopyHeight +
                  SubcanopyHeight +
                  Slope +
                  Aspect +
                  Elevation +
                  DistWater +
                  aug_ndvi_avg +
                  mean_temp +
                  sd_temp +
                  sd_humi +
                  NT_DIST_AVG +
                  NS_DIST_AVG +
                  GC_NG_AVG +
                  GC_NF_AVG +
                  GC_BS_AVG +
                  GC_SH_AVG, data = insect_df, studentize = T)

lm.bp

#Multicollinearity test - calculates the multicollinearity condition number (MCN): gives a dignose suggesting problems with the stability of the regression results due to multicollinearity. MCN between 5 and 10: low correlation; MCN higher than 30: moderate to high correlation

kappa(global_model$coefficients)

#For testing autocorrelation with Moran I -  Global Moran's I statistic, the null hypothesis states that the attribute being analyzed is randomly distributed among the features in your study area; said another way, the spatial processes promoting the observed pattern of values is random chance. When the p-value returned by this tool is statistically significant, you can reject the null hypothesis. Non significant p: You cannot reject the null hypothesis. It is quite possible that the spatial distribution of feature values is the result of random spatial processes. The observed spatial pattern of feature values could very well be one of many, many possible versions of complete spatial randomness (CSR). Significant p + positive z: You may reject the null hypothesis. The spatial distribution of high values and/or low values in the dataset is more spatially clustered than would be expected if underlying spatial processes were random. Significant p + negative z: You may reject the null hypothesis. The spatial distribution of high values and low values in the dataset is more spatially dispersed than would be expected if underlying spatial processes were random. A dispersed spatial pattern often reflects some type of competitive process—a feature with a high value repels other features with high values; similarly, a feature with a low value repels other features with low values. source: https://pro.arcgis.com/en/pro-app/tool-reference%20/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm#:~:text=the%20null%20hypothesis.-,Interpretation,context%20of%20its%20null%20hypothesis.&text=The%20table%20below%20summarizes%20interpretation,value%20is%20not%20statistically%20significant.
df_residuals <- cbind(insect_df, global_model$residuals)


# write.csv(df_residuals, getDataPath("Chapter1_FineScaleAcousticSurvey", "01.07.2020_dfresiduals.csv"))
# 
# 
# df_residuals <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "01.07.2020_dfresiduals.csv"))
# 
# df_residuals2 <- mutate(df_residuals, FID = gsub(X.1, pattern = "-", replacement = ""))
# df_residuals3 <- mutate(df_residuals2, FID = gsub(FID, pattern = "_", replacement = ""))
# write.csv(df_residuals3, getDataPath("Chapter1_FineScaleAcousticSurvey", "01.07.2020_dfresiduals.csv"))

c <- df_residuals$LAT
d <- df_residuals$LONG

f <- cbind(c, d)

b <- as.matrix(dist(f))

points.dist <- as.matrix(dist(b))
points.dist.inv <- 1/points.dist
diag(points.dist.inv) <- 0
points.dist.inv[is.infinite(points.dist.inv)] <- 0

points.dist.inv <- as.matrix(points.dist.inv)

library(spdep)

Moran.I(df_residuals$`global_model$residuals`, points.dist.inv) #p significant: the process is not random, there is a spatial component to it; positive observed z score: dataset is more spatially clustered than expected by chance.


