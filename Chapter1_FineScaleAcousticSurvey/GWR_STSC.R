#GWR#
library(GWmodel)
library(tidyverse)
library(sp)
library(purrr)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

df_veg <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "23.07.2020_scaledvars_complete.csv")) %>% 
  select(., c(3:22,30:50))
df_cluster <-  read.csv(getDataPath("STSC", "Results", "BOWpartitional5.csv"))

df_join <- merge(df_cluster, df_veg, by = c("FileName", "ResultMinute"))

#Standardizing independent variables
ind_variables <- df_join[c("temperature", "NDVI_AVERAGE", "CanopyCover", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH")]

#creating spatial df#
#cluster1#

cluster1 <- filter(df_join, cluster_number == "5")

LatLong <- cbind(cluster1$LAT, cluster1$LONG)


spatialdf <- SpatialPointsDataFrame(coords = LatLong, data = cluster1, proj4string = CRS("+proj=longlat +datum=WGS84"))

dist_matrix <- gw.dist(dp.locat = coordinates(spatialdf), p = 2)


lm.veg <- lm(index_value ~ NDVI_AVERAGE + CanopyCover + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf)

summary(lm.veg)


lm.temp <- lm(index_value ~ temperature, data = spatialdf)

summary(lm.temp)

#Multiple R-squared: model performance; Coefficients: each explanatory variable; F stats and p value = model significanceAdjusted R sqaured takes into account model complexity (number of variables) - more accurate measure of model performanceThe coefficient for each explanatory variable reflects both the strength and type of relationship the explanatory variable has to the dependent variable. When the sign associated with the coefficient is negative, the relationship is negative (for example, the larger the distance from the urban core, the smaller the number of residential burglaries). When the sign is positive, the relationship is positive (for example, the larger the population, the larger the number of residential burglaries). Coefficients are given in the same units as their associated explanatory variables (a coefficient of 0.005 associated with a variable representing population counts may be interpreted as 0.005 people). The coefficient reflects the expected change in the dependent variable for every 1-unit change in the associated explanatory variable, holding all other variables constant (for example, a 0.005 increase in residential burglary is expected for each additional person in the census block, holding all other explanatory variables constant). The T test is used to assess whether an explanatory variable is statistically significant. The null hypothesis is that the coefficient is, for all intents and purposes, equal to zero (and consequently is not helping the model). When the probability or robust probability (p-value) is very small, the chance of the coefficient being essentially zero is also small. An explanatory variable associated with a statistically significant coefficient is important to the regression model if theory or common sense supports a valid relationship with the dependent variable if the relationship being modeled is primarily linear, and if the variable is not redundant to any other explanatory variables in the model.#

library(lmtest)


lm.bp <- bptest(index_value ~ temperature + NDVI_AVERAGE + CanopyCover + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, studentize = T)


#Assess stationarity. The Koenker (BP) Statistic (Koenker's studentized Bruesch-Pagan statistic) is a test to determine whether the explanatory variables in the model have a consistent relationship to the dependent variable both in geographic space and in data space. When the model is consistent in geographic space, the spatial processes represented by the explanatory variables behave the same everywhere in the study area (the processes are stationary). When the model is consistent in data space, the variation in the relationship between predicted values and each explanatory variable does not change with changes in explanatory variable magnitudes (there is no heteroscedasticity in the model). Suppose you want to predict crime, and one of your explanatory variables is income. The model would have problematic heteroscedasticity if the predictions were more accurate for locations with small median incomes than they were for locations with large median incomes. The null hypothesis for this test is that the model is stationary. For a 95 percent confidence level, a p-value (probability) smaller than 0.05 indicates statistically significant heteroscedasticity and/or nonstationarity. When results from this test are statistically significant, consult the robust coefficient standard errors and probabilities to assess the effectiveness of each explanatory variable. Regression models with statistically significant nonstationarity are often good candidates for Geographically Weighted Regression (GWR) analysis.#

#Selecting bandwidth and kernel function#
#Testing with gaussian kernel#
library(RColorBrewer)

BD_sel <- bw.gwr(index_value ~ temperature + NDVI_AVERAGE + CanopyCover + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, approach = "AIC", kernel = "gaussian", adaptive = F, dMat = dist_matrix)

summary_stats <- gwss(data = spatialdf, vars = c("index_value", "temperature", "NDVI_AVERAGE", "CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH" ), bw = BD_sel, kernel = "gaussian", adaptive = F, dMat = dist_matrix)

model_selection <- model.selection.gwr(DeVar =  "index_value", InDeVars = c("temperature", "NDVI_AVERAGE", "CanopyCover", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = F, kernel = "gaussian", dMat = dist_matrix)
model_list <- model_selection[[1]]

model_view <- model.view.gwr(DeVar =  "index_value", InDeVars = c("temperature", "NDVI_AVERAGE", "CanopyCover", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), model.list = model_list)

model.list <- model.sort.gwr(model_selection, numVars = 12, ruler.vector = model_selection[[2]][,2])

GWR_model_selection <- plot(model.list[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#COmplete Model - seems to be the lower AIC value#

gwr.res <- gwr.basic(index_value ~ temperature + NDVI_AVERAGE + CanopyCover + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf,  bw = BD_sel, adaptive = F)

print(gwr.res)

#Optimized model#

gwr.res <- gwr.basic(index_value ~ Elevation + AVERAGE_NS_DIST, data = spatialdf,  bw = BD_sel, adaptive = F)

print(gwr.res)

model <- as.data.frame(gwr.res$SDF)


#Local r2 per point
ggplot(model, aes(x = as.factor(coords.x2), y = as.numeric(Local_R2))) +
  geom_col(position = "dodge") +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Local R2 values per point", x = "Point", y = "Local R2", fill = "Day period")
