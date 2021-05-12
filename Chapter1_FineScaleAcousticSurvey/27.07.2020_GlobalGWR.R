library(GWmodel)
library(tidyverse)
library(sp)
library(purrr)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
  
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

# landscape_var <- read.csv(getDataPath("Fieldwork_Bowra", "27.01.2021_Data.csv"))
# 
# labels <- read.csv(getDataPath(chapter, "DiscriminantAnalysis", "Bowraaug_class_RFlabels.csv"))%>% 
#   select(., id, class_model) %>%
#   droplevels(.)
# 
# indices <- read.csv(getDataPath("STSC", "Results", "Bowraaug", "Bowraaugmotif_complete.csv")) 
# 
# ndvi <- read.csv(getDataPath(chapter, "RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted", "26.02.2021_ndviavg.csv")) %>%
#   select(Point, aug_ndvi_avg) %>%
#   droplevels(.)
# 
# savi <- read.csv(getDataPath(chapter, "RemoteSensing", "2_SentinelHub_Landsat_SAVIExtracted", "26.02.2021_saviavg.csv")) %>%
#   select(Point, aug_savi_avg) %>%
#   droplevels(.)
# 
# hobos <- read.csv(getDataPath(chapter, "26.02.2021_hobo_aug.csv")) %>%
#   select(date, hour, minutes, Humidity, Temperature, point, hobo) %>%
#   group_by(point, date, hour) %>% 
#   summarise(., mean_temp = mean(Temperature), sd_temp = sd(Temperature), mean_humi = mean(Humidity), sd_humi = sd(Humidity)) %>% 
#   droplevels(.)
# 
# labelled_indices <- #select(labels, id, component_model, class_model) %>%
#   #left_join(., indices) %>%
#   select(indices, everything(), -fid_what) %>% 
#   group_by(id) %>% 
#   summarise(., mean = mean(index_value), sd = sd(index_value)) %>% 
#   left_join(indices, ., by = "id") %>% 
#   distinct(., id, .keep_all = T) %>% 
#   left_join(., labels) %>% 
#   filter(., class_model != "NA") %>% 
#   separate(., id, into = c("point", "index", "number", "what"), sep = "_", remove = F) %>% 
#   separate(., time, into = c("hour", "minute", "seconds"), sep = c(-4,-2), remove = F) %>%
#   filter(., point != "WAA20" & point != "WBA2O") %>% 
#   droplevels(.)
# 
# rm(labels, indices)
# 
# labelled_indices$point <- as.character(labelled_indices$point)
# 
# labelled_indices <- mutate(labelled_indices, new_point = case_when(point == "WA001" ~ "WA01",
#                                     point == "WA002" ~ "WA02",
#                                     point == "WA003" ~ "WA03",
#                                     point == "WA004" ~ "WA04",
#                                     point == "WA005" ~ "WA05",
#                                     point == "WA006" ~ "WA06",
#                                     point == "WA007" ~ "WA07",
#                                     point == "WA008" ~ "WA08",
#                                     point == "WA009" ~ "WA09",
#                                     point == "WA010" ~ "WA10",
#                                     point == "WA011" ~ "WA11",
#                                     point == "WA012" ~ "WA12",
#                                     point == "WA013" ~ "WA13",
#                                     point == "WA014" ~ "WA14",
#                                     point == "WA015" ~ "WA15",
#                                     point == "WA016" ~ "WA16",
#                                     point == "WA017" ~ "WA17",
#                                     point == "WA018" ~ "WA18",
#                                     point == "WA019" ~ "WA19",
#                                     point == "WA020" ~ "WA20",
#                                     point == "WA021" ~ "WA21",
#                                     point == "WA022" ~ "WA22",
#                                     point == "WA023" ~ "WA23",
#                                     point == "WA024" ~ "WA24",
#                                     point == "WB006" ~ "WB06",
#                                     point == "WB008" ~ "WB08",
#                                     point == "WB011" ~ "WB11",
#                                     point == "WB014" ~ "WB14",
#                                     point == "WB015" ~ "WB15",
#                                     point == "WB017" ~ "WB17",
#                                     point == "WB024" ~ "WB24",
#                                     point == "WB025" ~ "WB25",
#                                     point == "WB027" ~ "WB27",
#                                     point == "WB028" ~ "WB28",
#                                     point == "WB029" ~ "WB29",
#                                     point == "WB034" ~ "WB34",
#                                     point == "WB035" ~ "WB35",
#                                     point == "WB036" ~ "WB36",
#                                     point == "WB043" ~ "WB43",
#                                     point == "WB044" ~ "WB44",
#                                     point == "WB046" ~ "WB46",
#                                     point == "WB047" ~ "WB47",
#                                     point == "WB049" ~ "WB49",
#                                     point == "WB052" ~ "WB52",
#                                     point == "WB056" ~ "WB56",
#                                     point == "WB057" ~ "WB57",
#                                     point == "WB058" ~ "WB58")) %>% 
#   select(., everything(), -point) %>% 
#   rename(., "point" = new_point)%>%
#   droplevels(.)
# 
# labelled_indices$point <- as.factor(labelled_indices$point)
# 
# lab_ind_land <- left_join(labelled_indices, landscape_var, by = c("point" = "Point")) %>%
#   droplevels(.)
# 
# rm(labelled_indices, landscape_var)
# 
# land_ind_land_sat <- left_join(lab_ind_land, ndvi, by = c("point" = "Point"))
# land_ind_land_sat <- left_join(land_ind_land_sat, savi, by = c("point" = "Point"))
# 
# rm(lab_ind_land, ndvi, savi)
# 
# land_ind_land_sat <- mutate(land_ind_land_sat, minute = case_when(ResultMinute <= 9 ~ 0,
#                                                       10 >= ResultMinute | ResultMinute <= 19 ~ 10,
#                                                       20 >= ResultMinute | ResultMinute <= 29 ~ 20,
#                                                       30 >= ResultMinute | ResultMinute <= 39 ~ 30,
#                                                       40 >= ResultMinute | ResultMinute <= 49 ~ 40,
#                                                       50 <= ResultMinute ~ 50))
# 
# land_ind_land_sat$minute <- as.integer(land_ind_land_sat$minute)
# 
# land_ind_land_sat$hour <- as.integer(land_ind_land_sat$hour)
# 
# complete_df <- left_join(x = land_ind_land_sat, y = hobos, by = c("point" = "point", "date" = "date", "hour" = "hour")) %>% 
#   select(everything(), -X)
# 
# write.csv(complete_df, getDataPath(chapter, "27.02.2021_CompleteData.csv"), row.names = F)
# write.csv(hobos, getDataPath(chapter, "27.02.2021_hobos_average.csv"), row.names = F)
# 
# summary(complete_df)
# 
# rm(land_ind_land_sat, hobos)

complete_df <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv"))


# complete_df <- mutate_at(complete_df, vars(NT_N_DIST, NT_W_DIST, NT_S_DIST, NT_E_DIST, NS_N_DIST, NS_W_DIST, NS_S_DIST, NS_E_DIST), ~ replace(., is.na(.), 100)) %>% 
#   mutate_at(., vars(NT_N_HEIGHT, NT_S_HEIGHT, NT_W_HEIGHT, NT_E_HEIGHT, NS_N_HEIGHT, NS_S_HEIGHT, NS_E_HEIGHT, NS_W_HEIGHT), ~replace(., is.na(.), 0)) %>% 
#   mutate_at(., vars(GC_NF_W, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight), ~replace(., is.na(.), 0)) %>% 
#   mutate(NT_DIST_AVG = (NT_N_DIST + NT_S_DIST + NT_E_DIST + NT_W_DIST)/4) %>% 
#   mutate(NT_HEIGHT_AVG = (NT_N_HEIGHT + NT_S_HEIGHT + NT_E_HEIGHT + NT_W_HEIGHT)/4) %>% 
#   mutate(NS_DIST_AVG = (NS_N_DIST + NS_S_DIST + NS_E_DIST + NS_W_DIST)/4) %>% 
#   mutate(NS_HEIGHT_AVG = (NS_N_HEIGHT + NS_S_HEIGHT + NS_E_HEIGHT + NS_W_HEIGHT)/4) %>%
#   mutate(GC_NG_AVG = (GC_NG_N + GC_NG_S + GC_NG_E + GC_NG_W)/4) %>% 
#   mutate(GC_NF_AVG = (GC_NF_N + GC_NF_S + GC_NF_E + GC_NF_W)/4) %>% 
#   mutate(GC_BS_AVG = (GC_BS_N + GC_BS_S + GC_BS_E + GC_BS_W)/4) %>%
#   mutate(GC_LT_AVG = (GC_LT_N + GC_LT_S + GC_LT_E + GC_LT_W)/4) %>%
#   mutate(GC_SH_AVG = (GC_SH_N + GC_SH_S + GC_SH_E + GC_SH_W)/4)
# 
# write.csv(complete_df, getDataPath("Chapter1_FineScaleAcousticSurvey", "27.02.2021_CompleteData.csv"), row.names = F)
# 
# plot(complete_df[c(22,23,25)], fill = complete_df$class_model),26,28,29,31,32,34,35,37,38,40:65,67:73,80:84)])



complete_df <- complete_df %>% mutate_at(c(73:80, 87:101), scale)


#Bird analysis#
bird_df <- filter(complete_df, class_model == "bird") %>% 
  droplevels(.)

summary(bird_df)

#Removing highly correlated variables
cor <- cor(bird_df[c(73:80, 87:101)]) %>% 
  write.csv(getDataPath(chapter, "28.02.2021_birdcorrelation_landvariables.csv"))

bird_df <- select(bird_df, everything(), CanopyCover,
                        ShrubCover,
                        CanopyHeight,
                        SubcanopyHeight,
                        Slope,
                        Aspect,
                        Elevation,
                        DistWater,
                        aug_ndvi_avg,
                        mean_temp,
                        sd_temp,
                        sd_humi,
                        NT_DIST_AVG,
                        NS_DIST_AVG,
                        GC_NG_AVG,
                        GC_NF_AVG,
                        GC_BS_AVG,
                        GC_SH_AVG
                        )


#creating spatial df#
#Morning#
LatLong <- cbind(bird_df$LAT, bird_df$LONG)
spatialdf <- SpatialPointsDataFrame(coords = LatLong, data = bird_df, proj4string = CRS("+proj=longlat +datum=WGS84"))

dist_matrix <- gw.dist(dp.locat = coordinates(spatialdf), p = 2)

#Setting the dependent variable#
DeVar <- "mean"

#Setting indepentendent variables
IndVar <- c("CanopyCover",
                        "ShrubCover",
                        "CanopyHeight",
                        "SubcanopyHeight",
                        "Slope",
                        "Aspect",
                        "Elevation",
                        "DistWater",
                        "aug_ndvi_avg",
                        "mean_temp",
                        "sd_temp",
                        "sd_humi",
                        "NT_DIST_AVG",
                        "NS_DIST_AVG",
                        "GC_NG_AVG",
                        "GC_NF_AVG",
                        "GC_BS_AVG",
                        "GC_SH_AVG")


lm.global <- glm(sd ~ CanopyCover +
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
                 GC_SH_AVG
                  , data = bird_df, family = gaussian())

r.squaredGLMM(lm.global)

null <- glm(mean~1, data = bird_df)

anova(lm.global, null, test = "F")

anova(lm.global, test = "F")

lm.opt <- glm(mean ~ CanopyCover +
                ShrubCover +
                CanopyHeight +
                
                Slope +
                Aspect +
                
                DistWater +
                aug_ndvi_avg +
                mean_temp +
                sd_temp +
                sd_humi +
                NT_DIST_AVG +
                NS_DIST_AVG +
                
                GC_NF_AVG +
                GC_BS_AVG +
                GC_SH_AVG
              , data = bird_df, family = gaussian())

anova(lm.opt, null, test = "F")

anova(lm.opt, test = "F")

summary(lm.opt)

r.squaredGLMM(lm.opt)

3544.3/7885


3544.3/3871.6

#Multiple R-squared: model performance; Coefficients: each explanatory variable; F stats and p value = model significanceAdjusted R sqaured takes into account model complexity (number of variables) - more accurate measure of model performanceThe coefficient for each explanatory variable reflects both the strength and type of relationship the explanatory variable has to the dependent variable. When the sign associated with the coefficient is negative, the relationship is negative (for example, the larger the distance from the urban core, the smaller the number of residential burglaries). When the sign is positive, the relationship is positive (for example, the larger the population, the larger the number of residential burglaries). Coefficients are given in the same units as their associated explanatory variables (a coefficient of 0.005 associated with a variable representing population counts may be interpreted as 0.005 people). The coefficient reflects the expected change in the dependent variable for every 1-unit change in the associated explanatory variable, holding all other variables constant (for example, a 0.005 increase in residential burglary is expected for each additional person in the census block, holding all other explanatory variables constant). The T test is used to assess whether an explanatory variable is statistically significant. The null hypothesis is that the coefficient is, for all intents and purposes, equal to zero (and consequently is not helping the model). When the probability or robust probability (p-value) is very small, the chance of the coefficient being essentially zero is also small. An explanatory variable associated with a statistically significant coefficient is important to the regression model if theory or common sense supports a valid relationship with the dependent variable if the relationship being modeled is primarily linear, and if the variable is not redundant to any other explanatory variables in the model.#


#Bird analysis#
insect_df <- filter(complete_df, class_model == "insect") %>% 
  droplevels(.)



lab_ind_land_insect <- select(insect_df, everything(), NT_W_HEIGHT,
                       NS_N_DIST,
                       NS_S_DIST,
                       NS_W_DIST,
                       GC_NG_N,
                       GC_BS_N,
                       GC_LT_N,
                       GC_NG_E,
                       GC_SH_E,
                       GC_NG_S,
                       GC_SH_S,
                       GC_NG_W,
                       GC_NF_W,
                       GC_LT_W,
                       GC_SH_W,
                       CanopyCover,
                       ShrubCover,
                       CanopyHeight,
                       Slope,
                       Aspect,
                       Elevation,
                       NDVI_AVERAGE,
                       temperature,
)


lm.global <- glm(lab_ind_land_insect$index_value ~ lab_ind_land_insect$NS_N_DIST +
                   lab_ind_land_insect$NS_W_DIST +
                   lab_ind_land_insect$GC_BS_N +
                   lab_ind_land_insect$GC_LT_N +
                   lab_ind_land_insect$GC_NG_E +
                   lab_ind_land_insect$GC_NG_W +
                   lab_ind_land_insect$GC_NF_W +
                   lab_ind_land_insect$CanopyCover +
                   lab_ind_land_insect$ShrubCover +
                   lab_ind_land_insect$CanopyHeight +
                   lab_ind_land_insect$Aspect +
                   lab_ind_land_insect$temperature +
                   lab_ind_land_insect$NDVI_AVERAGE
                 , data = lab_ind_land_insect, family = gaussian())

null <- glm(lab_ind_land_insect$index_value~1)

anova(lm.global, null, test = "F")

anova(lm.global, test = "F")

lm.opt <- glm(lab_ind_land_insect$index_value ~ lab_ind_land_insect$NS_N_DIST +
                lab_ind_land_insect$NS_W_DIST +
                lab_ind_land_insect$GC_BS_N +
                lab_ind_land_insect$GC_LT_N +
                lab_ind_land_insect$GC_NG_E +
                lab_ind_land_insect$GC_NG_W +

                lab_ind_land_insect$CanopyCover +
                lab_ind_land_insect$ShrubCover +
                lab_ind_land_insect$CanopyHeight +
                lab_ind_land_insect$Aspect +
                lab_ind_land_insect$temperature +
                lab_ind_land_insect$NDVI_AVERAGE
              , data = lab_ind_land, family = gaussian())

anova(lm.global, lm.opt, test = "F")

anova(lm.opt, test = "F")

summary(lm.opt)

#ratio between the residual deviance and the degrees of freedom - from residual deviance. The value of this ratio should be like one of the outcomes of the model: "Dispersion parameter for gaussian family taken to be...". This message you'll only appear once you've run the summary(result). If the result is similar, the distribution is adequate and you can move forward. If not, you'll have to play with the families to test which one is good for you.

1490.0/5199

#  Calculate the ratio between test model deviance and the residual deviance of the null model (this parameters should appear in the ANOVA outcome.

1490.0/1765.1

#Multiple R-squared: model performance; Coefficients: each explanatory variable; F stats and p value = model significanceAdjusted R sqaured takes into account model complexity (number of variables) - more accurate measure of model performanceThe coefficient for each explanatory variable reflects both the strength and type of relationship the explanatory variable has to the dependent variable. When the sign associated with the coefficient is negative, the relationship is negative (for example, the larger the distance from the urban core, the smaller the number of residential burglaries). When the sign is positive, the relationship is positive (for example, the larger the population, the larger the number of residential burglaries). Coefficients are given in the same units as their associated explanatory variables (a coefficient of 0.005 associated with a variable representing population counts may be interpreted as 0.005 people). The coefficient reflects the expected change in the dependent variable for every 1-unit change in the associated explanatory variable, holding all other variables constant (for example, a 0.005 increase in residential burglary is expected for each additional person in the census block, holding all other explanatory variables constant). The T test is used to assess whether an explanatory variable is statistically significant. The null hypothesis is that the coefficient is, for all intents and purposes, equal to zero (and consequently is not helping the model). When the probability or robust probability (p-value) is very small, the chance of the coefficient being essentially zero is also small. An explanatory variable associated with a statistically significant coefficient is important to the regression model if theory or common sense supports a valid relationship with the dependent variable if the relationship being modeled is primarily linear, and if the variable is not redundant to any other explanatory variables in the model.#

select(lab_ind_land_insect, c(index_value, NS_N_DIST, NS_W_DIST, GC_BS_N, GC_LT_N, GC_NG_E, GC_NG_W, CanopyCover, ShrubCover, CanopyHeight, Aspect , temperature, NDVI_AVERAGE, LAT, LONG)) %>% write.csv(., getDataPath("Chapter1_FineScaleAcousticSurvey", "insect_test.csv"))



library(lmtest)


lm.bp <- bptest(index_value ~ CanopyCover +
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
                  GC_SH_AVG
                , data = bird_df, studentize = T)

lm.bp


#Assess stationarity. The Koenker (BP) Statistic (Koenker's studentized Bruesch-Pagan statistic) is a test to determine whether the explanatory variables in the model have a consistent relationship to the dependent variable both in geographic space and in data space. When the model is consistent in geographic space, the spatial processes represented by the explanatory variables behave the same everywhere in the study area (the processes are stationary). When the model is consistent in data space, the variation in the relationship between predicted values and each explanatory variable does not change with changes in explanatory variable magnitudes (there is no heteroscedasticity in the model). Suppose you want to predict crime, and one of your explanatory variables is income. The model would have problematic heteroscedasticity if the predictions were more accurate for locations with small median incomes than they were for locations with large median incomes. The null hypothesis for this test is that the model is stationary. For a 95 percent confidence level, a p-value (probability) smaller than 0.05 indicates statistically significant heteroscedasticity and/or nonstationarity. When results from this test are statistically significant, consult the robust coefficient standard errors and probabilities to assess the effectiveness of each explanatory variable. Regression models with statistically significant nonstationarity are often good candidates for Geographically Weighted Regression (GWR) analysis.#

#Global Model#
#Selecting bandwidth#
BD_sel <- bw.ggwr(index_value ~ NS_N_DIST +
                   NS_W_DIST +
                   GC_BS_N +
                   GC_LT_N +
                   GC_NG_E +
                   GC_NG_W +
                   GC_NF_W +
                   CanopyCover +
                   ShrubCover +
                   CanopyHeight +
                   Slope +
                   Aspect +
                   Elevation +
                   NDVI_AVERAGE +
                   temperature, data = spatialdf, approach = "AIC", kernel = "tricube", adaptive = F, dMat = dist_matrix)

model_selection <- model.selection.gwr(DeVar, IndVar, data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = F, kernel = "tricube", dMat = dist_matrix)

sorted_models <- model.sort.gwr(model_selection, numVars = length(IndVar), ruler.vector = model_selection[[2]][,2])

model_list <- sorted_models[[1]]

model_view <- model.view.gwr(DeVar, IndVar, model.list = model_list)

GWR_model_selection <- plot(sorted_models[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#COmplete Model - seems to be the lower AIC value#

gwr.res <- gwr.basic(Ndsi ~ CanopyCover + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH + NDVI_AVERAGE, data = spatialdf,  bw = BD_sel, adaptive = F, kernel = "tricube")
summary(gwr.res$lm)

model <- as.data.frame(gwr.res$SDF)


model_grouped <- group_by(model, "coords.x2" = signif(coords.x2, digits = 7)) %>% 
  summarise_all(., mean) %>% 
  select(coords.x1, coords.x2, IndVar, everything())

#Local r2 per point
ggplot(model, aes(x = as.factor(coords.x2), y = Local_R2)) +
  geom_col(position = "dodge") +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Local R2 values per point - Global and and Ndsi", x = "Point", y = "Local R2", fill = "Day period") +
ggsave(getDataPath("Figures", "27.07.2020_gwrr2valuespersite_globalmodel_Ndsi.tiff"))

df_gather <- gather(model_grouped[,1:14], -c(coords.x1, coords.x2), key = "coefficient_name", value = "coefficient_value")

ggplot(df_gather, aes(x = as.factor(coords.x2), y = coefficient_value, fill = coefficient_name)) +
  geom_col(position = "dodge") +
  theme_classic() +
  scale_fill_manual(values = c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b", "#40004b")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Coefficients per point - Global model and Ndsi", x = "Point", y = "Coefficients Value", fill = "Variables") +
  ggsave(getDataPath("Figures", "27.07.2020_Coefficients_globalmodel_Ndsi.tiff"))


#Global model with temperature and time#
BD_sel <- bw.gwr(Ndsi ~ temperature, data = spatialdf, approach = "AIC", kernel = "tricube", adaptive = F, dMat = dist_matrix)

model_selection <- model.selection.gwr(DeVar =  "Ndsi", InDeVars = c("temperature"), data = spatialdf, bw = BD_sel, approach = "AIC", adaptive = F, kernel = "tricube", dMat = dist_matrix)

model_list <- model.sort.gwr(model_selection, numVars = 1, ruler.vector = model_selection[[2]][,2])

model_view <- model.view.gwr(DeVar =  "Ndsi", InDeVars = c("temperature", "beginning_rec_modified"), model.list = model_list)

GWR_model_selection <- plot(model.list[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

gwr.res <- gwr.basic(Ndsi ~ temperature, data = spatialdf,  bw = BD_sel, adaptive = F, kernel = "tricube")
summary(gwr.res$lm)

model <- as.data.frame(gwr.res$SDF)


#Local r2 per point
ggplot(model, aes(x = as.factor(coords.x2), y = Local_R2)) +
  geom_col(position = "dodge") +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Local R2 values per point - Temperature and Ndsi", x = "Point", y = "Local R2") +
  ggsave(getDataPath("Figures", "27.07.2020_gwrr2valuespersite_temptimemodel_Ndsi.tiff"))

model_grouped <- group_by(model, "coords.X2" = signif(coords.x2, digits = 7)) %>% 
  summarise_all(., mean)


ggplot(model_grouped, aes(x = as.factor(coords.x2), y = temperature)) +
  geom_col(position = "dodge") +
  theme_classic() +
  scale_fill_manual(values = c("#7f3b08", "#b35806")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Temperature coefficients per point - Ndsi", x = "Point", y = "Coefficients Value", fill = "Variables") +
  ggsave(getDataPath("Figures", "27.07.2020_gwrtempcoefficients_Ndsi.tiff"))


