#GWR#

library(GWmodel)
library(tidyverse)
library(sp)
library(purrr)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "14.01.2019_glm_preparation.csv"))

#Variable treating#

list <- c(df$NT_E_DIST, df$NT_W_DIST, df$NT_E_DIST, df$NT_N_DIST)
df1 <- df %>% 
  mutate(NT_W_DIST = replace_na(NT_W_DIST, 0)) %>% 
  mutate(NT_E_DIST = replace_na(NT_E_DIST, 0)) %>% 
  mutate(NT_S_DIST = replace_na(NT_S_DIST, 0)) %>% 
  mutate(NT_N_DIST = replace_na(NT_N_DIST, 0))

df2 <- df1 %>% 
  mutate(NS_W_DIST = replace_na(NS_W_DIST, 0)) %>% 
  mutate(NS_E_DIST = replace_na(NS_E_DIST, 0)) %>% 
  mutate(NS_S_DIST = replace_na(NS_S_DIST, 0)) %>% 
  mutate(NS_N_DIST = replace_na(NS_N_DIST, 0))

rm(df, df1, df_1)

df3 <- df2 %>% 
  mutate(GC_NG_N = replace_na(GC_NG_N, 0)) %>% 
  mutate(GC_NF_N = replace_na(GC_NF_N, 0)) %>% 
  mutate(GC_BS_N = replace_na(GC_BS_N, 0)) %>% 
  mutate(GC_LT_N = replace_na(GC_LT_N, 0)) %>% 
  mutate(GC_SH_N = replace_na(GC_SH_N, 0)) %>% 
  mutate(GC_SH_E = replace_na(GC_SH_E, 0))
rm(df2)

df3 <- df3 %>% 
  mutate(GC_SH_S = replace_na(GC_SH_S, 0)) %>% 
  mutate(GC_NF_W = replace_na(GC_NF_W, 0)) %>% 
  mutate(GC_SH_W = replace_na(GC_SH_W, 0))

df_1 <- df3 %>% 
  mutate(AVERAGE_NT_DIST = (NT_E_DIST+NT_W_DIST+NT_S_DIST+NT_N_DIST)/4) %>% 
  mutate(AVERAGE_NS_DIST = (NS_E_DIST+NS_W_DIST+NS_S_DIST+NS_N_DIST)/4) %>% 
  mutate(AVERAGE_GC_NG = (GC_NG_N+GC_NG_S+GC_NG_E+GC_NG_W)/4) %>% 
  mutate(AVERAGE_GC_NF = (GC_NF_N+GC_NF_S+GC_NF_E+GC_NF_W)/4) %>% 
  mutate(AVERAGE_GC_BS = (GC_BS_N+GC_BS_S+GC_BS_E+GC_BS_W)/4) %>% 
  mutate(AVERAGE_GC_LT = (GC_LT_N+GC_LT_S+GC_LT_E+GC_LT_W)/4) %>% 
  mutate(AVERAGE_GC_SH = (GC_SH_N+GC_SH_S+GC_SH_E+GC_SH_W)/4)
rm(df3)

df_1 <- df_1 %>%
  mutate(NT_E_HEIGHT = replace_na(NT_E_HEIGHT, 0)) %>% 
  mutate(NT_W_HEIGHT = replace_na(NT_W_HEIGHT, 0)) %>% 
  mutate(NT_S_HEIGHT = replace_na(NT_S_HEIGHT, 0)) %>% 
  mutate(NT_N_HEIGHT = replace_na(NT_N_HEIGHT, 0)) %>% 
  mutate(NS_E_HEIGHT = replace_na(NS_E_HEIGHT, 0)) %>% 
  mutate(NS_W_HEIGHT = replace_na(NS_W_HEIGHT, 0)) %>% 
  mutate(NS_S_HEIGHT = replace_na(NS_S_HEIGHT, 0)) %>% 
  mutate(NS_N_HEIGHT = replace_na(NS_N_HEIGHT, 0)) %>%  
  mutate(AVERAGE_NT_HEIGHT = (NT_E_HEIGHT+NT_W_HEIGHT+NT_S_HEIGHT+NT_N_HEIGHT)/4) %>% 
  mutate(AVERAGE_NS_HEIGHT = (NS_E_HEIGHT+NS_W_HEIGHT+NS_S_HEIGHT+NS_N_HEIGHT)/4)

df_1 <- df_1 %>% 
  mutate(GC_NG_N = replace_na(GC_NG_N, 0)) %>%
  mutate(GC_NG_S = replace_na(GC_NG_S, 0)) %>% 
  mutate(GC_NG_E = replace_na(GC_NG_E, 0)) %>% 
  mutate(GC_NG_W = replace_na(GC_NG_W, 0)) %>% 
  mutate(GC_NF_N = replace_na(GC_NF_N, 0)) %>%
  mutate(GC_NF_S = replace_na(GC_NF_S, 0)) %>% 
  mutate(GC_NF_E = replace_na(GC_NF_E, 0)) %>% 
  mutate(GC_NF_W = replace_na(GC_NF_W, 0)) %>%
  mutate(GC_LT_N = replace_na(GC_LT_N, 0)) %>%
  mutate(GC_LT_S = replace_na(GC_LT_S, 0)) %>% 
  mutate(GC_LT_E = replace_na(GC_LT_E, 0)) %>% 
  mutate(GC_LT_W = replace_na(GC_LT_W, 0)) %>%
  mutate(AVERAGE_GC_NG = (GC_NG_N+GC_NG_S+GC_NG_E+GC_NG_W)/4) %>%
  mutate(AVERAGE_GC_NF = (GC_NF_N+GC_NF_S+GC_NF_E+GC_NF_W)/4) %>%
  mutate(AVERAGE_GC_LT = (GC_LT_N+GC_LT_S+GC_LT_E+GC_LT_W)/4)

#Standardizing independent variables
ind_variables <- df_1[c("CanopyCover", "ShrubCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_BS", "AVERAGE_GC_LT", "AVERAGE_GC_SH", "AVERAGE_NT_HEIGHT", "AVERAGE_NS_HEIGHT")]

other_variables <- df_1[c("PointData", "X.1", "categorical_time_4groups", "categorical_time_8groups", "dominant_sound", "second_dominant", "obs", "time", "BackgroundNoise", "HighFreqCover", "LowFreqCover", "AcousticComplexity", "EntropyOfVarianceSpectrum", "ClusterCount", "Ndsi", "FileName", "Point", "Date", "beginning_rec", "Recorder", "ResultMinute", "HOBO", "NT_N_SP", "NT_S_SP", "NT_E_SP", "NT_W_SP", "NS_N_SP", "NS_S_SP", "NS_E_SP", "NS_W_SP", "CanopyEmergent", "LandformDescription", "VegDescription", "LAT", "LONG", "beginning_rec_modified")]

stand_df <- scale(ind_variables)

stand_df <- as.data.frame(stand_df)
plot(stand_df)
summary(stand_df)

final_df <- cbind(stand_df, other_variables)

#Testing data normality#
library(ggpubr)

CanopyCover <- ggqqplot(stand_df$CanopyCover, ylab = "Canopy Cover", title = "Canopy Cover normality plot")
ShrubCover <- ggqqplot(stand_df$ShrubCover, ylab = "Shrub Cover", title = "Shrub Cover normality plot")
CanopyHeight <- ggqqplot(stand_df$CanopyHeight, ylab = "Canopy Height", title = "Canopy Height normality plot")
SubcanopyHeight <- ggqqplot(stand_df$SubcanopyHeight, ylab = "Subcanopy Height", title = "Subcanopy Height normality plot")
Slope <- ggqqplot(stand_df$Slope, ylab = "Slope", title = "Slope normality plot")
Aspect <- ggqqplot(stand_df$Aspect, ylab = "Aspect", title = "Aspect normality plot")
Elevation <- ggqqplot(stand_df$Elevation, ylab = "Elevation", title = "Elevation normality plot")
AVERAGE_NT_DIST <- ggqqplot(stand_df$AVERAGE_NT_DIST, ylab = "Average Nearest Tree Distance", title = "Average Nearest Tree Distance normality plot")
AVERAGE_NS_DIST <- ggqqplot(stand_df$AVERAGE_NS_DIST, ylab = "Average Nearest Shrub Distance", title = "Average Nearest Shrub Distance normality plot")
AVERAGE_GC_NG <- ggqqplot(stand_df$AVERAGE_GC_NG, ylab = "Average Ground Cover Native Grass", title = "Average Ground Cover Native Grass normality plot")
AVERAGE_GC_NF <- ggqqplot(stand_df$AVERAGE_GC_NF, ylab = "Average Ground Cover Native Forbs", title = "Average Ground Cover Native Forbs normality plot")
AVERAGE_GC_BS <- ggqqplot(stand_df$AVERAGE_GC_BS, ylab = "Average Ground Cover Bare Soil", title = "Average Ground Cover Bare Soil normality plot")
AVERAGE_GC_LT <- ggqqplot(stand_df$AVERAGE_GC_LT, ylab = "Average Ground Cover Litter", title = "Average Ground Cover Litter normality plot")
AVERAGE_GC_SH <- ggqqplot(stand_df$AVERAGE_GC_SH, ylab = "Average Ground Cover Shrub", title = "Average Ground Cover Shrub normality plot")
AVERAGE_NT_HEIGHT <- ggqqplot(stand_df$AVERAGE_NT_HEIGHT, ylab = "Average Nearest Tree Height", title = "Average Nearest Tree Height normality plot")
AVERAGE_NS_HEIGHT <- ggqqplot(stand_df$AVERAGE_NS_HEIGHT, ylab = "Average Nearest Shrub Height", title = "Average Nearest Shrub Height normality plot")

ggexport(CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, AVERAGE_NT_DIST, AVERAGE_NS_DIST, AVERAGE_GC_NG, AVERAGE_GC_NF, AVERAGE_GC_BS, AVERAGE_GC_LT, AVERAGE_GC_SH, AVERAGE_NT_HEIGHT, AVERAGE_NS_HEIGHT, filename = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/NormalityPlot_standardizedVariables.tiff")

#Correlation on explicative variables - First round removed: Shrub cover, Subcanopy Height, Average NT Height, Average NS Height, Average GC litter, Average GC Bare soil - Recheck#

cor <- abs(cor(stand_df, use = "complete.obs", method = "spearman"))
#write.csv(cor, getDataPath("Fieldwork_Bowra", "Oct2019", "SummaryIndices_Channel1_Prepared", "10.04.2020_standardisedindvariables.csv"))

test <- stand_df[c(1,3:11,14)]

cor <- abs(cor(test, use = "complete.obs", method = "spearman"))
#write.csv(cor, getDataPath("Fieldwork_Bowra", "Oct2019", "SummaryIndices_Channel1_Prepared", "10.04.2020_standardisedindvariables3.csv"))

final_df <- cbind(test, other_variables)
write.csv(final_df, getDataPath("Fieldwork_Bowra", "Oct2019", "SummaryIndices_Channel1_Prepared", "12.04.2020_gwrdata.csv"))
#Separating df into 4 categorical times#
final_df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "SummaryIndices_Channel1_Prepared", "12.04.2020_gwrdata.csv"))

  ggplot(final_df, aes(x = as.factor(LONG), y = AcousticComplexity)) +
  geom_col(position = "dodge", aes(fill = categorical_time_4groups)) +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Acoustic Complexity per point", x = "Point", y = "Acoustic Complexity", fill = "Day period") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) +
  facet_wrap(~ categorical_time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_AcousticComplexityperSite.tiff"))

df_2 <- filter(final_df, final_df$categorical_time_4groups == "afternoon")


final_df %>% filter(categorical_time_4groups == "morning") %>% 
ggplot(., aes(x = as.factor(LONG), y = AcousticComplexity)) +
  geom_col(position = "dodge", aes(fill = categorical_time_4groups)) +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Morning Acoustic Complexity per point", x = "Point", y = "Acoustic Complexity", fill = "Day period") +
  scale_fill_manual(values = "#ffffb3", labels = "Morning") +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_AcousticComplexityperSite_morning.tiff"))

final_df %>% filter(categorical_time_4groups == "afternoon") %>% 
  ggplot(., aes(x = as.factor(LONG), y = AcousticComplexity)) +
  geom_col(position = "dodge", aes(fill = categorical_time_4groups)) +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Afternoon Acoustic Complexity per point", x = "Point", y = "Acoustic Complexity", fill = "Day period") +
  scale_fill_manual(values = "#fb8072", labels = "Afternoon") +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_AcousticComplexityperSite_Afternoon.tiff"))

final_df %>% filter(categorical_time_4groups == "evening") %>% 
  ggplot(., aes(x = as.factor(LONG), y = AcousticComplexity)) +
  geom_col(position = "dodge", aes(fill = categorical_time_4groups)) +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Evening Acoustic Complexity per point", x = "Point", y = "Acoustic Complexity", fill = "Day period") +
  scale_fill_manual(values = "#80b1d3", labels = "Evening") +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_AcousticComplexityperSite_evening.tiff"))

final_df %>% filter(categorical_time_4groups == "night") %>% 
  ggplot(., aes(x = as.factor(LONG), y = AcousticComplexity)) +
  geom_col(position = "dodge", aes(fill = categorical_time_4groups)) +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Night Acoustic Complexity per point", x = "Point", y = "Acoustic Complexity", fill = "Day period") +
  scale_fill_manual(values = "#bc80bd", labels = "Night") +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_AcousticComplexityperSite_night.tiff"))

df_2 <- filter(final_df, final_df$categorical_time_4groups == "morning")

#creating spatial df#
#Morning#
LatLong <- cbind(df_2$LAT, df_2$LONG)
spatialdf <- SpatialPointsDataFrame(coords = LatLong, data = df_2, proj4string = CRS("+proj=longlat +datum=WGS84"))

dist_matrix <- gw.dist(dp.locat = coordinates(spatialdf), p = 2)


lm.global <- lm(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf)

summary(lm.global)

#Multiple R-squared: model performance; Coefficients: each explanatory variable; F stats and p value = model significanceAdjusted R sqaured takes into account model complexity (number of variables) - more accurate measure of model performanceThe coefficient for each explanatory variable reflects both the strength and type of relationship the explanatory variable has to the dependent variable. When the sign associated with the coefficient is negative, the relationship is negative (for example, the larger the distance from the urban core, the smaller the number of residential burglaries). When the sign is positive, the relationship is positive (for example, the larger the population, the larger the number of residential burglaries). Coefficients are given in the same units as their associated explanatory variables (a coefficient of 0.005 associated with a variable representing population counts may be interpreted as 0.005 people). The coefficient reflects the expected change in the dependent variable for every 1-unit change in the associated explanatory variable, holding all other variables constant (for example, a 0.005 increase in residential burglary is expected for each additional person in the census block, holding all other explanatory variables constant). The T test is used to assess whether an explanatory variable is statistically significant. The null hypothesis is that the coefficient is, for all intents and purposes, equal to zero (and consequently is not helping the model). When the probability or robust probability (p-value) is very small, the chance of the coefficient being essentially zero is also small. An explanatory variable associated with a statistically significant coefficient is important to the regression model if theory or common sense supports a valid relationship with the dependent variable if the relationship being modeled is primarily linear, and if the variable is not redundant to any other explanatory variables in the model.#

library(lmtest)


lm.bp <- bptest(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, studentize = T)


#Assess stationarity. The Koenker (BP) Statistic (Koenker's studentized Bruesch-Pagan statistic) is a test to determine whether the explanatory variables in the model have a consistent relationship to the dependent variable both in geographic space and in data space. When the model is consistent in geographic space, the spatial processes represented by the explanatory variables behave the same everywhere in the study area (the processes are stationary). When the model is consistent in data space, the variation in the relationship between predicted values and each explanatory variable does not change with changes in explanatory variable magnitudes (there is no heteroscedasticity in the model). Suppose you want to predict crime, and one of your explanatory variables is income. The model would have problematic heteroscedasticity if the predictions were more accurate for locations with small median incomes than they were for locations with large median incomes. The null hypothesis for this test is that the model is stationary. For a 95 percent confidence level, a p-value (probability) smaller than 0.05 indicates statistically significant heteroscedasticity and/or nonstationarity. When results from this test are statistically significant, consult the robust coefficient standard errors and probabilities to assess the effectiveness of each explanatory variable. Regression models with statistically significant nonstationarity are often good candidates for Geographically Weighted Regression (GWR) analysis.#

#Selecting bandwidth and kernel function#
#Testing with gaussian kernel#
library(RColorBrewer)

BD_sel <- bw.gwr(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, approach = "AIC", kernel = "gaussian", adaptive = F, dMat = dist_matrix)

summary_stats <- gwss(data = spatialdf, vars = c("AcousticComplexity", "CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH" ), bw = BD_sel, kernel = "gaussian", adaptive = F, dMat = dist_matrix)

morning_summarydf <- as.data.frame(summary_stats$SDF)
  
morning_df_gather <- gather(night_summarydf, -c(coords.x1, coords.x2), key = "Stat", value = "Value") %>% 
  filter(Stat == "AcousticComplexity_LSD" | Stat == "CanopyCover_LSD" | Stat == "CanopyHeight_LSD" | Stat == "SubcanopyHeight_LSD" | Stat == "Slope_LSD" | Stat == "Aspect_LSD" | Stat == "Elevation_LSD" | Stat == "AVERAGE_NT_DIST_LSD" | Stat == "AVERAGE_NS_DIST_LSD" | Stat == "AVERAGE_GC_NG_LSD" | Stat == "AVERAGE_GC_NF_LSD" | Stat == "AVERAGE_GC_SH_LSD")

night_df_gather$time_4groups <- "night"
morning_df_gather$time_4groups <- "morning"
afternoon_df_gather$time_4groups <- "afternoon"
evening_df_gather$time_4groups <- "evening"

df_SDall <- rbind(evening_df_gather, morning_df_gather, afternoon_df_gather, evening_df_gather)
write.csv(df_SDall, getDataPath("Chapter1_FineScaleAcousticSurvey", "05.05.2020_spatialstats.csv"))

ggplot(evening_df_gather, aes(x = coords.x2, y = Value, colour = Stat)) +
  geom_line() #+
  #facet_wrap(~ Stat)

model_selection <- model.selection.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = F, kernel = "gaussian", dMat = dist_matrix)
model_list <- model_selection[[1]]

model_view <- model.view.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), model.list = model_list)

model.list <- model.sort.gwr(model_selection, numVars = 11, ruler.vector = model_selection[[2]][,2])

GWR_model_selection <- plot(model.list[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#COmplete Model - seems to be the lower AIC value#

gwr.res <- gwr.basic(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf,  bw = BD_sel, adaptive = F)

evening_model <- as.data.frame(gwr.res$SDF)

night_model$time_4groups <- "night"
afternoon_model$time_4groups <- "afternoon"
morning_model$time_4groups <- "morning"
evening_model$time_4groups <- "evening"

all_model <- rbind(night_model, afternoon_model, morning_model, evening_model)

all_model$time_4groups <- all_model$time_4groups[order(all_model$Local_R2)]

#Local r2 per point
ggplot(all_model, aes(x = as.factor(coords.x2), y = as.numeric(Local_R2))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Local R2 values per point", x = "Point", y = "Local R2", fill = "Day period") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) +
  facet_wrap(~ time_4groups) #+
  #ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_gwrr2valuespersite_separatetime.tiff"))
  

df_gather_evening <- gather(evening_model, -c(coords.x1, coords.x2), key = "coefficient_name", value = "coeffiecient_value")
unique(df_gather_afternoon$coefficient_name)

evening_coefficients <- filter(df_gather_evening, coefficient_name == "CanopyCover"  | coefficient_name == "CanopyHeight" | coefficient_name == "SubcanopyHeight" | coefficient_name == "Slope" | coefficient_name == "Aspect" | coefficient_name == "Elevation" | coefficient_name == "AVERAGE_NT_DIST" | coefficient_name == "AVERAGE_NS_DIST" | coefficient_name == "AVERAGE_GC_NG" | coefficient_name == "AVERAGE_GC_NF" | coefficient_name == "AVERAGE_GC_SH" | coefficient_name == "y" | coefficient_name == "Local_R2")


afternoon_coefficients %>% filter(coefficient_name == "y") %>% 
  ggplot(., aes(x = as.factor(coords.x2), y = coeffiecient_value)) +
  geom_violin() +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Afternoon Y variation per point", x = "Point", y = "Y") +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "14.04.2020_gwrYvaluespersite_afternoon.tiff"))

evening_coefficients %>% filter(coefficient_name != "y") %>% 
  ggplot(., aes(x = coords.x2, y = coeffiecient_value, colour = coefficient_name)) +
  geom_point() #+
  #facet_wrap(~ time_4groups)

night_coefficients$time_4groups <- "night"
morning_coefficients$time_4groups <- "morning"
afternoon_coefficients$time_4groups <- "afternoon"
evening_coefficients$time_4groups <- "evening"

coefficients_all <- rbind(night_coefficients, morning_coefficients, afternoon_coefficients, evening_coefficients)
write.csv(coefficients_all, getDataPath("Fieldwork_Bowra", "Oct2019", "SummaryIndices_Channel1_Prepared", "12.04.2020_gwrcoefficients.csv"))

coefficients_all <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "SummaryIndices_Channel1_Prepared", "12.04.2020_gwrcoefficients.csv"))

unique(coefficients_all$coefficient_name)
signif(as.numeric(coefficients_all$coeffiecient_value, 2))

labs <- c("Aspect" = "Aspect",  "AVERAGE_GC_NF" = "Ground Cover - Native Forbs", "AVERAGE_GC_NG" = "Ground Cover - Native Grass", "AVERAGE_GC_SH" ="Ground Cover - Shrub", "AVERAGE_NS_DIST" = "Distance to Nearest Shrub", "AVERAGE_NT_DIST" = "Distance to Nearest Tree", "CanopyCover" = "Canopy Cover", "CanopyHeight" = "Canopy Height", "Elevation" = "Elevation", "Slope" = "Slope", "SubcanopyHeight" = "Subcanopy Height")



#Morning coeffcients#
coefficients_all %>% filter(time_4groups == "evening" & coefficient_name != "Local_R2" & coefficient_name != "y") %>% 
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value), fill = coefficient_name)) +
  geom_col(position = "dodge") +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Evening coefficients per point", x = "Point", y = "Coefficients Value", fill = "Variables") #+
  #facet_wrap(~ coefficient_name, labeller = labeller(coefficient_name = labs)) +
  #ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "28.04.2020_GWReveningcoefficients.tiff"))

#Morning coeffcients#
coefficients_all %>% filter(time_4groups == "night" & coefficient_name != "Local_R2" & coefficient_name != "y") %>% 
  ggplot(., aes(x = as.factor(signif(coords.x2, digits = 7)), y = as.numeric(coeffiecient_value), fill = coefficient_name)) +
  geom_col(position = "dodge") +
  theme_classic() +
  scale_fill_manual(values = c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"), labels = labs) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Night coefficients per point", x = "Point", y = "Coefficients Value", fill = "Variables") +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "05.05.2020_NightCoefficientsperpoint.tiff"))

coefficients_all %>% filter(time_4groups == "night") %>% 
  unique(coefficients_all$coords.x2)

#Canopy Cover#
coefficients_all %>% filter(coefficient_name == "CanopyCover") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Canopy cover coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRcanopycoverperpoint_separatetimes.tiff"))

#CanopyHeight#
coefficients_all %>% filter(coefficient_name == "CanopyHeight") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Canopy height coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRcanopyheightperpoint_separatetimes.tiff"))

#SubcanopyHeight#
coefficients_all %>% filter(coefficient_name == "SubcanopyHeight") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Subcanopy height coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRsubcanopyheightperpoint_separatetimes.tiff"))

#Slope#
coefficients_all %>% filter(coefficient_name == "Slope") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Slope coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRSlopeperpoint_separatetimes.tiff"))

#Aspect#
coefficients_all %>% filter(coefficient_name == "Aspect") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Aspect coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRAspectperpoint_separatetimes.tiff"))

#Elevation#
coefficients_all %>% filter(coefficient_name == "Elevation") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Elevation coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRElevationperpoint_separatetimes.tiff"))

#AVERAGE_NT_DIST#
coefficients_all %>% filter(coefficient_name == "AVERAGE_NT_DIST") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Distance to nearest tree coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRAVERAGE_NT_DISTperpoint_separatetimes.tiff"))

#AVERAGE_NS_DIST#
coefficients_all %>% filter(coefficient_name == "AVERAGE_NS_DIST") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Distance to nearest shrub coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRAVERAGE_NS_DISTperpoint_separatetime.tiff"))

#AVERAGE_GC_NG#
coefficients_all %>% filter(coefficient_name == "AVERAGE_GC_NG") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Ground Cover (native grass) coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRAVERAGE_GC_NGperpoint_separatetime.tiff"))

#AVERAGE_GC_NF#
coefficients_all %>% filter(coefficient_name == "AVERAGE_GC_NF") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Ground Cover (native forbs) coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRAVERAGE_GC_NFperpoint_separatetime.tiff"))

#AVERAGE_GC_SH#
coefficients_all %>% filter(coefficient_name == "AVERAGE_GC_SH") %>%
  ggplot(., aes(x = as.factor(coords.x2), y = as.numeric(coeffiecient_value))) +
  geom_col(position = "dodge", aes(fill = time_4groups)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#fb8072", "#80b1d3", "#ffffb3", "#bc80bd"), labels = c("Afternoon", "Evening", "Morning", "Night")) + 
  labs(title = "Ground Cover (shrub) coefficients per point", x = "Point", y = "Coefficients", fill = "Day period") +
  facet_wrap(~ time_4groups) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", "15.04.2020_GWRAVERAGE_GC_SHperpoint_separatetime.tiff"))

ggplot(coefficients_all, aes(x = coords.x2, y = CanopyCover, colour = time_4groups)) +
  geom_line(size = 1) +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Afternoon Y variation per point", x = "Point", y = "Y") #+
  #ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "14.04.2020_gwrYvaluespersite_afternoon.tiff"))


glm <- glm(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = df_2, family = gaussian())
null.model <- glm(AcousticComplexity ~ 1, data = df_2)

anova <- anova(null.model, glm, test="F") #Use this one if you use the gaussian family
summary(anova)
summary(glm) #residual deviance and the degrees of freedom - from residual deviance - checking if the family is corrected
2147.1/2217 #ok

#pseudo r2: Calculate the ratio between test model deviance and the residual deviance of the null model (this parameters should appear in the ANOVA outcome#
747.63/2894.7

#Same explanation as the linear model...#
names(gwr.res$SDF)
spplot(gwr.res$SDF, "Aspect")



anova(M.BGN100M, MN.BGN, test="chisq") #Use this one if you use the poisson family


summary(glm)

print(gwr.res)

qqnorm(lm.global$residuals)
qqnorm(lm.global$residuals)
shapiro.test(lm.global$residuals)
heteroce_test <- bptest(gwr.res$SDF$residual)


#Testing with exponential#
BD_sel <- bw.gwr(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, approach = "AIC", kernel = "exponential", adaptive = T, dMat = dist_matrix)

colli_test <- gwr.collin.diagno(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, kernel = "exponential", adaptive = T, dMat = dist_matrix, bw = BD_sel)

summary_stats <- gwss(data = spatialdf, vars = c("AcousticComplexity", "CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), bw = BD_sel, kernel = "exponential", adaptive = T, dMat = dist_matrix, quantile = T)

model_selection <- model.selection.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = T, kernel = "exponential", dMat = dist_matrix)
model_list <- model_selection[[1]]

model.view.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), model.list = model_list)

model.list <- model.sort.gwr(model_selection, numVars = 11, ruler.vector = model_selection[[2]][,2])

plot(model.list[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#Complete Model#

gwr.res <- gwr.basic(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, bw = BD_sel, kernel = "exponential", adaptive = T)
print(gwr.res)

#Testing with bisquare#
BD_sel <- bw.gwr(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, approach = "AIC", kernel = "bisquare", adaptive = T, dMat = dist_matrix)

summary_stats <- gwss(data = spatialdf, vars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), bw = BD_sel, kernel = "bisquare", adaptive = T, dMat = dist_matrix, quantile = T)

model_selection <- model.selection.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = T, kernel = "bisquare", dMat = dist_matrix)
model_list <- model_selection[[1]]

model.view.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), model.list = model_list)

model.list <- model.sort.gwr(model_selection, numVars = 11, ruler.vector = model_selection[[2]][,2])

plot(model.list[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#Complete Model#

gwr.res <- gwr.basic(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, bw = BD_sel, kernel = "bisquare", adaptive = T)
print(gwr.res)

#51 - BEST ONE

gwr.res <- gwr.basic(AcousticComplexity ~ Slope + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, bw = BD_sel, kernel = "bisquare", adaptive = T)

#Testing with tricube#
BD_sel <- bw.gwr(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, approach = "AIC", kernel = "tricube", adaptive = T, dMat = dist_matrix)

summary_stats <- gwss(data = spatialdf, vars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), bw = BD_sel, kernel = "tricube", adaptive = T, dMat = dist_matrix, quantile = T)

model_selection <- model.selection.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = T, kernel = "tricube", dMat = dist_matrix)
model_list <- model_selection[[1]]

model.view.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), model.list = model_list)

model.list <- model.sort.gwr(model_selection, numVars = 11, ruler.vector = model_selection[[2]][,2])

plot(model.list[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#Complete Model#

gwr.res <- gwr.basic(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, bw = BD_sel, kernel = "tricube", adaptive = T)

print(gwr.res)

#56
gwr.res <- gwr.basic(AcousticComplexity ~ CanopyHeight + SubcanopyHeight + Slope + Aspect + AVERAGE_NT_DIST + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, bw = BD_sel, kernel = "tricube", adaptive = T, dMat = dist_matrix)

#Testing with boxcar#
BD_sel <- bw.gwr(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, approach = "AIC", kernel = "boxcar", adaptive = T, dMat = dist_matrix)

summary_stats <- gwss(data = spatialdf, vars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), bw = BD_sel, kernel = "boxcar", adaptive = T, dMat = dist_matrix, quantile = T)

model_selection <- model.selection.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = T, kernel = "boxcar", dMat = dist_matrix)
model_list <- model_selection[[1]]

model.view.gwr(DeVar =  "AcousticComplexity", InDeVars = c("CanopyCover", "CanopyHeight", "SubcanopyHeight", "Slope", "Aspect", "Elevation", "AVERAGE_NT_DIST", "AVERAGE_NS_DIST", "AVERAGE_GC_NG", "AVERAGE_GC_NF", "AVERAGE_GC_SH"), model.list = model_list)

model.list <- model.sort.gwr(model_selection, numVars = 11, ruler.vector = model_selection[[2]][,2])

plot(model.list[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#Complete Model#

gwr.res <- gwr.basic(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, bw = BD_sel, kernel = "boxcar", adaptive = T)
print(gwr.res)

#According to the results, the kernel dist that best fitted the models was gaussian. although some models were better than others, and none of them achieved a r2 above .4, we need to closely inspect the results and see if maybe some points are better explained by the variables than others. Also, including other variables such as climatic, distance to water among other may be provide better results.

#GGWR#

bw.ggwr <- ggwr.sel(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, family = "gaussian")


GGWR <- ggwr(AcousticComplexity ~ CanopyCover + CanopyHeight + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH, data = spatialdf, bandwidth = bw.ggwr, family = "gaussian")

bw.ggwr.null <- ggwr.sel(AcousticComplexity ~ 1, data = spatialdf, family = "gaussian")
GGWR.null <- ggwr(AcousticComplexity ~ 1, data = spatialdf, bandwidth = bw.ggwr.null, family = "gaussian")
