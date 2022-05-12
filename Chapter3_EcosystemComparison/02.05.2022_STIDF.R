#Spatio-temporal models

# Data manipulation, transformation and visualisation
library(tidyverse)
# Nice tables
library(kableExtra)
# Simple features (a standardised way to encode vector data ie. points, lines, polygons)
library(sf) 
# Spatial objects conversion
library(sp) 
# Thematic maps
library(tmap) 
# Nice colour schemes
library(viridis) 
# Obtain correlation coefficients
library(corrplot)
# Highlight data on plots
library(gghighlight)
# Analysing spatio-temporal data
#library(STRbook)
library(spacetime)
# Date parsing and manipulation
library(lubridate)
# Applied statistics
library(MASS)
# Statistical tests for linear regression models
library(lmtest)
# Fit spatial random effects models
library(FRK)
# Exportable regression tables
library(jtools)
library(ape)

rm(list = ls())

set.seed(123)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("01.05.2022_fulldata.csv"))

data_og$date_r <- ymd(data_og$date_r)
data_og$day <- day(data_og$date_r)
data_og$week <- week(data_og$date_r)
data_og$month <- month(data_og$date_r)
data_og$year <- year(data_og$date_r)
data_og$bvg

data_og <- mutate(data_og, bvg = case_when(bvg == 98 ~ 9,
                                           TRUE ~ bvg))

data_og <- mutate(data_og, bvg_char = case_when(bvg == 2 ~ "tropical_rainforest",
                              bvg == 4 ~ "euc_open_shruby_under",
                              bvg == 8 ~ "euc_wood_shruby_under", 
                              bvg == 9 ~ "euc_wood_grassy_under",
                              bvg == 20 ~ "mulga_wood_grass_forbs",
                              bvg == 31 ~ "saltbush_shrub",
                              bvg == 62 ~ "dry_rainforest"))

data_og$end_time <- as.POSIXct(paste(data_og$date_r, data_og$Recording_time))

spatial <- st_as_sf(x = dplyr::select(data_og, c(RFclass, ID.x, lat, lon, 8:40)), coords = c("lat", "lon"), crs = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temporal <- dplyr::select(data_og, end_time)
data <- dplyr::select(data_og, c(3:5, 8, 41:48))

data_st <- stConstruct(x = data_og, space = c("lat", "lon"), time = "end_time")

aus_map <- st_read("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/MappingFiles_Aus/AllStatesPlygon_3857/AllStatesPolygon_3857.shp")


baselayer <- tm_shape(aus_map) +
  tm_borders()
rm(baselayer)

legend_title = expression("Number of motifs per location/class")
tm_shape(spatial) +
  tm_bubbles(size = 1, col = "n") +
  #tm_tiles(aus_map) +
  tm_facets(by = "RFclass", ncol = 3) +
  tm_borders(aus_map)
  tm_layout(bg.color = "white", # change background colour
            legend.outside = TRUE, # legend outside
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 2,
            legend.width = 1,
            legend.height = 1,
            panel.label.size = 3)
  
data_og <- separate(data_og, col = ID.x, into = c("site", "point"), sep = "_", remove = F)
  
(tsp <- ggplot(data = data_og,
                mapping = aes(x = date_r, y = n, group = site)))
(tsp + geom_line(color = "blue") + 
    gghighlight(max(n) > 5, use_direct_label = FALSE) +
    labs(title= paste(" "), x="Date", y="Number of motifs") +
    theme_classic() +
    theme(plot.title=element_text(size = 20)) +
    theme(axis.text=element_text(size=16)) +
    theme(axis.title.y = element_text(size = 18)) +
    theme(axis.title.x = element_text(size = 18)) +
    theme(plot.subtitle=element_text(size = 16)) +
    theme(axis.title=element_text(size=20, face="plain")) +
    facet_wrap(~ RFclass))

#After checking the time series, it is clear that tags bat, batbirdinsect, batfroginsect, birdmammal, frogbird and froginsectmammal don't have more than 5 motifs across sites, therefore they will be excluded from analysis from now on - tags kept = bird, birdfrog, birdfroginsect, birdinsect, frog, froginsect, insect

data_og <- filter(data_og, RFclass == "bird" | RFclass == "birdfrog" | RFclass == "birdfroginsect" | RFclass == "birdinsect" | RFclass == "frog" | RFclass == "froginsect" | RFclass == "insect")

(tsp <- ggplot(data = data_og,
               mapping = aes(x = date_r, y = n, group = site)))
(tsp + geom_line(color = "blue") + 
    gghighlight(max(n) > 5, use_direct_label = FALSE) +
    labs(title= paste(" "), x="Date", y="Number of motifs") +
    theme_classic() +
    theme(plot.title=element_text(size = 20)) +
    theme(axis.text=element_text(size=16)) +
    theme(axis.title.y = element_text(size = 18)) +
    theme(axis.title.x = element_text(size = 18)) +
    theme(plot.subtitle=element_text(size = 16)) +
    theme(axis.title=element_text(size=20, face="plain")) +
    facet_wrap(~ RFclass))


spatial <- st_as_sf(x = data_og, coords = c("lat", "lon"), crs = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

data_st <- stConstruct(x = data_og, space = c("lat", "lon"), time = "end_time")

aus_map <- st_as_sf(aus_map)

august_map <- spatial %>% filter(month == 8) %>% 
  tm_shape(.) +
  tm_bubbles(size = 1, col = "n") +
  #tm_tiles(aus_map) +
  tm_facets(by = "RFclass", ncol = 1) +
  #tm_borders(aus_map)
  tm_layout(bg.color = "white", # change background colour
          legend.outside = TRUE, # legend outside
          legend.outside.position = "right",
          legend.stack = "horizontal",
          legend.title.size = 2,
          legend.width = 1,
          legend.height = 1,
          panel.label.size = 2,
          main.title = "August")

september_map <- spatial %>% filter(month == 9) %>% 
  tm_shape(.) +
  tm_bubbles(size = 1, col = "n") +
  #tm_tiles(aus_map) +
  tm_facets(by = "RFclass", ncol = 1) +
  #tm_borders(aus_map)
  tm_layout(bg.color = "white", # change background colour
            legend.outside = TRUE, # legend outside
            legend.outside.position = "right",
            legend.stack = "horizontal",
            legend.title.size = 2,
            legend.width = 1,
            legend.height = 1,
            panel.label.size = 2,
            main.title = "September")

(october_map <- spatial %>% filter(month == 10) %>% 
  tm_shape(.) +
  tm_bubbles(size = 1, col = "n") +
  #tm_tiles(aus_map) +
  tm_facets(by = "RFclass", ncol = 1) +
  #tm_borders(aus_map)
  tm_layout(bg.color = "white", # change background colour
            legend.outside = TRUE, # legend outside
            legend.outside.position = "right",
            legend.stack = "horizontal",
            legend.title.size = 2,
            legend.width = 1,
            legend.height = 1,
            panel.label.size = 2,
            main.title = "October"))

anim <- tmap_animation(august_map, width = 3, height = 3, delay = 100, loop = T)


#For testing autocorrelation with Moran I -  Global Moran's I statistic, the null hypothesis states that the attribute being analyzed is randomly distributed among the features in your study area; said another way, the spatial processes promoting the observed pattern of values is random chance. When the p-value returned by this tool is statistically significant, you can reject the null hypothesis. Non significant p: You cannot reject the null hypothesis. It is quite possible that the spatial distribution of feature values is the result of random spatial processes. The observed spatial pattern of feature values could very well be one of many, many possible versions of complete spatial randomness (CSR). Significant p + positive z: You may reject the null hypothesis. The spatial distribution of high values and/or low values in the dataset is more spatially clustered than would be expected if underlying spatial processes were random. Significant p + negative z: You may reject the null hypothesis. The spatial distribution of high values and low values in the dataset is more spatially dispersed than would be expected if underlying spatial processes were random. A dispersed spatial pattern often reflects some type of competitive process—a feature with a high value repels other features with high values; similarly, a feature with a low value repels other features with low values. source: https://pro.arcgis.com/en/pro-app/tool-reference%20/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm#:~:text=the%20null%20hypothesis.-,Interpretation,context%20of%20its%20null%20hypothesis.&text=The%20table%20below%20summarizes%20interpretation,value%20is%20not%20statistically%20significant.

global_model <- lm(n ~ ca_class_1_3k + ca_class_9_3k + np_landscape_3k + temp_max + rain_value + moon_illu + period + bvg_char, data = data_og)

summary(global_model)
  
memory.limit(size = 20000)

df_residuals <- cbind(data_og, global_model$residuals)

c <- df_residuals$lat
d <- df_residuals$lon

f <- cbind(c, d)

b <- as.matrix(dist(f))

points.dist <- as.matrix(dist(b))
points.dist.inv <- 1/points.dist
diag(points.dist.inv) <- 0
points.dist.inv[is.infinite(points.dist.inv)] <- 0

Moran.I(df_residuals$`global_model$residuals`, points.dist.inv) #p significant: the process is not random, there is a spatial component to it; positive observed z score: dataset is more spatially clustered than expected by chance.

library(lmtest)
library(GWmodel)

spatial$bvg_char <- as.factor(spatial$bvg_char)
lm.bp <- bptest(n ~ np_landscape_3k + temp_max + rain_value + moon_illu + period + month + bvg_char, data = data_og, studentize = T)

lm.bp


#Assess stationarity. The Koenker (BP) Statistic (Koenker's studentized Bruesch-Pagan statistic) is a test to determine whether the explanatory variables in the model have a consistent relationship to the dependent variable both in geographic space and in data space. When the model is consistent in geographic space, the spatial processes represented by the explanatory variables behave the same everywhere in the study area (the processes are stationary). When the model is consistent in data space, the variation in the relationship between predicted values and each explanatory variable does not change with changes in explanatory variable magnitudes (there is no heteroscedasticity in the model). Suppose you want to predict crime, and one of your explanatory variables is income. The model would have problematic heteroscedasticity if the predictions were more accurate for locations with small median incomes than they were for locations with large median incomes. The null hypothesis for this test is that the model is stationary. For a 95 percent confidence level, a p-value (probability) smaller than 0.05 indicates statistically significant heteroscedasticity and/or nonstationarity. When results from this test are statistically significant, consult the robust coefficient standard errors and probabilities to assess the effectiveness of each explanatory variable. 
#Regression models with statistically significant nonstationarity are often good candidates for Geographically Weighted Regression (GWR) analysis.#

DeVar <- "n"

IndVar <- c("np_landscape_3k" , "temp_max" , "rain_value" , "moon_illu", "month", "bvg_char")


spatial <- as(spatial, Class = "Spatial")

dist_matrix <- gw.dist(dp.locat = coordinates(spatial), p = 2)

#Global Model#
#Selecting bandwidth#
#BD_sel <- bw.ggwr(n ~ bvg, data = spatial, approach = "AIC", kernel = "tricube", adaptive = F, dMat = dist_matrix)
BD_sel <- bw.ggwr(n ~ np_landscape_3k + temp_max + rain_value + moon_illu + month + bvg_char, data = spatial, approach = "AIC", kernel = "tricube", adaptive = F, dMat = dist_matrix)

model_selection <- model.selection.gwr(DeVar, IndVar, data = spatial, bw = BD_sel, approach = "AIC", adaptive = F, kernel = "tricube", dMat = dist_matrix)

sorted_models <- model.sort.gwr(model_selection, numVars = length(IndVar), ruler.vector = model_selection[[2]][,2])

model_list <- sorted_models[[1]]

model_view <- model.view.gwr(DeVar, IndVar, model.list = model_list)

GWR_model_selection <- plot(sorted_models[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure", ylab = "AICc", xlab = "Model number", type = "b")

#COmplete Model - seems to be the lower AIC value#

gwr.res <- gwr.basic(n ~ np_landscape_3k + temp_max + rain_value + moon_illu + period + month, data = spatial,  bw = BD_sel, adaptive = F, kernel = "tricube")
summary(gwr.res$lm)

model <- as.data.frame(gwr.res$SDF)


model_grouped <- group_by(model, "coords.x2" = signif(coords.x2, digits = 7)) %>% 
  summarise_all(., mean) %>% 
  dplyr::select(all_of(IndVar), everything())

#Local r2 per point
ggplot(model, aes(x = site, y = Local_R2)) +
  geom_col(position = "dodge") +
  theme_classic() +
  theme(panel.border = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  labs(title = "Local R2 values per point", x = "Point", y = "Local R2", fill = "Period")
  ggsave(getDataPath("Figures", "02.05.2022_gwrr2valuespersite_globalmodel.tiff"))

df_gather <- gather(model_grouped[,1:14], -c(lat, lon), key = "coefficient_name", value = "coefficient_value")

ggplot(df_gather, aes(x = site, y = coefficient_value, fill = coefficient_name)) +
  geom_col(position = "dodge") +
  theme_classic() +
  scale_fill_manual(values = c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b", "#40004b")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Coefficients per point - Global model", x = "Site", y = "Coefficients Value", fill = "Variables") +
  ggsave(getDataPath("Figures", "02.05.2022_Coefficients_globalmodel_Ndsi.tiff"))

