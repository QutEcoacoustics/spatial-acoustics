library(tidyverse)
library(randomForest)
library(party)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

indices <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv"))

# write.csv(land_var, getDataPath("8.11.2021_test.csv"))
# 
# df_newveg1 <- select(land_var, Point, NewVegDescription, VegDescription2) %>% 
#   merge(., nmds_df, by.x = "Point", by.y = "point") %>% 
#   mutate_at(c(75:82, 89, 91, 95, 97, 99:103), decostand, method = "range")

 # summary_stats <- group_by(indices, id) %>% 
 #  summarise(., mean = mean(index_value), sd = sd(index_value)) %>% 
 #   left_join(indices, ., by = "id") %>% 
 #  distinct(., id, .keep_all = T)

summary_stats$LAT <- signif(summary_stats$LONG, 7)

summary_stats <- summary_stats %>% mutate_at(c(70:77, 84, 85, 87:99), scale)

birds <- filter(summary_stats, class_model == "bird") %>% 
  droplevels(.)

#General Model

cor <- cor(summary_stats[c(70:77, 84, 85, 87:99)])








set.seed(123)

ct <- ctree(class_model ~ CanopyCover + ShrubCover + CanopyHeight + SubcanopyHeight + Elevation + aug_ndvi_avg + NT_DIST_AVG + NS_DIST_AVG + GC_NG_AVG + GC_NF_AVG + GC_BS_AVG + GC_SH_AVG + DistWater, data=indices, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))

print(ct)

pClassId <- predict(ct)

# check predicted classes against original class labels

table(indices$class_model, pClassId)

#accuracy

(sum(indices$class_model==pClassId)) / nrow(indices)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))

model_df <- select(indices, class_model, CanopyCover, ShrubCover , CanopyHeight , SubcanopyHeight , aug_ndvi_avg , NT_DIST_AVG , NS_DIST_AVG , GC_NG_AVG , GC_NF_AVG , GC_BS_AVG , GC_SH_AVG , DistWater, mean_temp)

model_df$period <- as.factor(model_df$period)

summary(model_df)

model_df <- filter(model_df, class_model == "insect")

train_index <- sample(1:nrow(model_df), 0.6 * nrow(model_df))
test_index <- setdiff(1:nrow(model_df), train_index)

train <- model_df[train_index,]%>%
  droplevels(.)


test <- model_df[test_index,]%>%
  droplevels(.)

rf <- randomForest(class_model ~ CanopyCover + ShrubCover + CanopyHeight + SubcanopyHeight + aug_ndvi_avg + NT_DIST_AVG + NS_DIST_AVG + GC_NG_AVG + GC_NF_AVG + GC_BS_AVG + GC_SH_AVG + DistWater + mean_temp, data=model_df, importance = T, proximity = T)#, mtry = best.m)

predict <- predict(rf, test)

summary(predict)

print(rf)

summary(rf$rsq)

varImpPlot(rf)

#Optimising

importance <- as.data.frame(randomForest::importance(rf)) %>%
  #filter(., IncNodePurity > 2) %>%
  row.names(.)
# 
model_data <- select(rf, mean, all_of(importance)) %>%
  droplevels(.)

floor(sqrt(ncol(model_df) - 1))

mtry <- tuneRF(model_df[-1],model_df$class_model, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf <- randomForest(class_model ~ ., data=model_df, importance = T, proximity = T, mtry = best.m)

print(rf)

varImpPlot(rf)

