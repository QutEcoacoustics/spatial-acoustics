library(tidyverse)
library(caTools)
library(lmtest)
library(ape)


#Pre GWR - testing assumptions and preparing data

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

df <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "08.06.2020_completedata.csv"))

#Splitting data into training and testing
data_split <- sample.split(df, SplitRatio = 0.75)
train <- subset(df, data_split == TRUE)
test <- subset(df, data_split == FALSE)


df_filtered <- filter(df, categorical_time_4groups == "evening")

#Linear Model
global_model <- lm(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = df_filtered)

summary(global_model)

#Heterocedasticidade test - evaluates if there is non constant variance in the model errors; p value significant means that the relationship between explanatory and response variables are non stationary
lm.bp <- bptest(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = df_filtered, studentize = T)

lm.bp

#Multicollinearity test - calculates the multicollinearity condition number (MCN): gives a dignose suggesting problems with the stability of the regression results due to multicollinearity. MCN between 5 and 10: low correlation; MCN higher than 30: moderate to high correlation

kappa(global_model$coefficients)

#For testing autocorrelation with Moran I -  Global Moran's I statistic, the null hypothesis states that the attribute being analyzed is randomly distributed among the features in your study area; said another way, the spatial processes promoting the observed pattern of values is random chance. When the p-value returned by this tool is statistically significant, you can reject the null hypothesis. Non significant p: You cannot reject the null hypothesis. It is quite possible that the spatial distribution of feature values is the result of random spatial processes. The observed spatial pattern of feature values could very well be one of many, many possible versions of complete spatial randomness (CSR). Significant p + positive z: You may reject the null hypothesis. The spatial distribution of high values and/or low values in the dataset is more spatially clustered than would be expected if underlying spatial processes were random. Significant p + negative z: You may reject the null hypothesis. The spatial distribution of high values and low values in the dataset is more spatially dispersed than would be expected if underlying spatial processes were random. A dispersed spatial pattern often reflects some type of competitive process—a feature with a high value repels other features with high values; similarly, a feature with a low value repels other features with low values. source: https://pro.arcgis.com/en/pro-app/tool-reference%20/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm#:~:text=the%20null%20hypothesis.-,Interpretation,context%20of%20its%20null%20hypothesis.&text=The%20table%20below%20summarizes%20interpretation,value%20is%20not%20statistically%20significant.
df_residuals <- cbind(df_filtered, global_model$residuals)

c <- df_residuals$LAT
d <- df_residuals$LONG

f <- cbind(c, d)

b <- as.matrix(dist(f))

points.dist <- as.matrix(dist(b))
points.dist.inv <- 1/points.dist
diag(points.dist.inv) <- 0
points.dist.inv[is.infinite(points.dist.inv)] <- 0

Moran.I(df_residuals$`global_model$residuals`, points.dist.inv) #p significant: the process is not random, there is a spatial component to it; positive observed z score: dataset is more spatially clustered than expected by chance.


