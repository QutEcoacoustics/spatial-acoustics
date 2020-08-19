library(GWmodel)
library(tidyverse)
library(sp)
library(purrr)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey",  ...))
  
}

df <- read.csv(getDataPath("23.07.2020_scaledvars_complete.csv"))
summary(df)

#creating spatial df#
#Morning#
LatLong <- cbind(df$LAT, df$LONG)
spatialdf <- SpatialPointsDataFrame(coords = LatLong, data = df, proj4string = CRS("+proj=longlat +datum=WGS84"))

dist_matrix <- gw.dist(dp.locat = coordinates(spatialdf), p = 2)

#Setting the dependent variable#
DeVar <- "ClusterCount"

#Setting indepentendent variables
IndVar <- c("CanopyCover", "SubcanopyHeight", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "NDVI_AVERAGE")

lm.global <- lm(DeVar ~ IndVar, data = df)


lm.global <- lm(ClusterCount ~ CanopyCover + SubcanopyHeight + AVERAGE_NT_DIST + AVERAGE_NS_DIST + NDVI_AVERAGE, data = df)

summary(lm.global)

#Multiple R-squared: model performance; Coefficients: each explanatory variable; F stats and p value = model significanceAdjusted R sqaured takes into account model complexity (number of variables) - more accurate measure of model performanceThe coefficient for each explanatory variable reflects both the strength and type of relationship the explanatory variable has to the dependent variable. When the sign associated with the coefficient is negative, the relationship is negative (for example, the larger the distance from the urban core, the smaller the number of residential burglaries). When the sign is positive, the relationship is positive (for example, the larger the population, the larger the number of residential burglaries). Coefficients are given in the same units as their associated explanatory variables (a coefficient of 0.005 associated with a variable representing population counts may be interpreted as 0.005 people). The coefficient reflects the expected change in the dependent variable for every 1-unit change in the associated explanatory variable, holding all other variables constant (for example, a 0.005 increase in residential burglary is expected for each additional person in the census block, holding all other explanatory variables constant). The T test is used to assess whether an explanatory variable is statistically significant. The null hypothesis is that the coefficient is, for all intents and purposes, equal to zero (and consequently is not helping the model). When the probability or robust probability (p-value) is very small, the chance of the coefficient being essentially zero is also small. An explanatory variable associated with a statistically significant coefficient is important to the regression model if theory or common sense supports a valid relationship with the dependent variable if the relationship being modeled is primarily linear, and if the variable is not redundant to any other explanatory variables in the model.#

library(lmtest)


lm.bp <- bptest(ClusterCount ~ CanopyCover + SubcanopyHeight + AVERAGE_NT_DIST + AVERAGE_NS_DIST + NDVI_AVERAGE, data = spatialdf, studentize = T)

lm.bp


#Assess stationarity. The Koenker (BP) Statistic (Koenker's studentized Bruesch-Pagan statistic) is a test to determine whether the explanatory variables in the model have a consistent relationship to the dependent variable both in geographic space and in data space. When the model is consistent in geographic space, the spatial processes represented by the explanatory variables behave the same everywhere in the study area (the processes are stationary). When the model is consistent in data space, the variation in the relationship between predicted values and each explanatory variable does not change with changes in explanatory variable magnitudes (there is no heteroscedasticity in the model). Suppose you want to predict crime, and one of your explanatory variables is income. The model would have problematic heteroscedasticity if the predictions were more accurate for locations with small median incomes than they were for locations with large median incomes. The null hypothesis for this test is that the model is stationary. For a 95 percent confidence level, a p-value (probability) smaller than 0.05 indicates statistically significant heteroscedasticity and/or nonstationarity. When results from this test are statistically significant, consult the robust coefficient standard errors and probabilities to assess the effectiveness of each explanatory variable. Regression models with statistically significant nonstationarity are often good candidates for Geographically Weighted Regression (GWR) analysis.#

#Global Model#
#Selecting bandwidth#
BD_sel <- bw.gwr(ClusterCount ~ CanopyCover + SubcanopyHeight + AVERAGE_NT_DIST + AVERAGE_NS_DIST + NDVI_AVERAGE, data = spatialdf, approach = "AIC", kernel = "tricube", adaptive = F, dMat = dist_matrix)

model_selection <- model.selection.gwr(DeVar, IndVar, data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = F, kernel = "tricube", dMat = dist_matrix)

sorted_models <- model.sort.gwr(model_selection, numVars = length(IndVar), ruler.vector = model_selection[[2]][,2])

model_list <- sorted_models[[1]]

model_view <- model.view.gwr(DeVar, IndVar, model.list = model_list)

GWR_model_selection <- plot(sorted_models[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#COmplete Model - seems to be the lower AIC value#

gwr.res <- gwr.basic(ClusterCount ~ CanopyCover + SubcanopyHeight + AVERAGE_NT_DIST + AVERAGE_NS_DIST + NDVI_AVERAGE, data = spatialdf,  bw = BD_sel, adaptive = F, kernel = "tricube")
summary(gwr.res$lm)

model <- as.data.frame(gwr.res$SDF)


model_grouped <- group_by(model, "coords.X2" = signif(coords.x2, digits = 7)) %>% 
  summarise_all(., mean) %>% 
  select(coords.x1, coords.x2, IndVar, everything())

#Local r2 per point
ggplot(model, aes(x = as.factor(coords.x2), y = Local_R2)) +
  geom_col(position = "dodge") +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Local R2 values per point - Canopy and ClusterCount Model", x = "Point", y = "Local R2", fill = "Day period") +
  ggsave(getDataPath("Figures", "05.08.2020_gwrr2valuespersite_canopymodel_ClusterCount.tiff"))

df_gather <- gather(model_grouped[,1:7], -c(coords.x1, coords.x2), key = "coefficient_name", value = "coefficient_value")

ggplot(df_gather, aes(x = as.factor(coords.x2), y = coefficient_value, fill = coefficient_name)) +
  geom_col(position = "dodge") +
  theme_classic() +
  scale_fill_manual(values = c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b", "#40004b")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Coefficients per point - Canopy and ClusterCount model", x = "Point", y = "Coefficients Value", fill = "Variables") +
ggsave(getDataPath("Figures", "05.08.2020_Coefficients_canopymodel_ClusterCount.tiff"))

