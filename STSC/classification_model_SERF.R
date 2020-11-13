library(tidyverse)
library(caret)
library(repr)
library(data.table)
library(TTR)
library(forecast)
library(lubridate)
library(randomForest)
library(wavelets)
library(party)
library(vegan)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

# #df with the motifs and filenames

cluster_results <- read.csv(getDataPath("STSC", "Results", "matchmotif_SERF_complete.csv"))


# #cluster_results <- mutate(cluster_results, name_img_file = paste(cluster_results$id, cluster_results$FileName, sep = "_"))
#
#
# #Randomly selecting motifs to inspect
#
#
#
# # extracting DWT coefficients (with Haar filter)
ts_data <- select(cluster_results, index_value, position, id) %>%
  group_by(., id) %>%
  mutate(., new_position = order(order(position))) %>%
  #mutate(., new_time = min(time)) %>%
  ungroup(.) %>%
  select(., everything(), -position) %>%
  pivot_wider(., names_from = new_position, values_from = index_value) %>%
  as.data.frame(.)


rownames(ts_data) <- ts_data$id
ts_data <- ts_data[,3:length(ts_data)]
#
ts_data[is.na.data.frame(ts_data)] <- 0

sample_size <- ceiling(nrow(ts_data)*0.05)

sampling <- sample(rownames(ts_data), size = sample_size, replace = F)

wtData <- NULL

for (i in 1:nrow(ts_data)) {

  a <- t(ts_data[i,])

  wt <- dwt(a, filter="haar", boundary= "periodic")

  wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))

}

wtData <- as.data.frame(wtData)
rownames(wtData) <- rownames(ts_data)

write.csv(wtData, getDataPath("Chapter2_SoundscapeTemporalAssessment", "DiscriminantAnalysis", "wavelet_serf_0.csv"), row.names = T)

#After inspecting motifs, load the df with labelled data

labelled_bowra <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", "wavelet_bow_0_labelled_simple.csv"))
rownames(labelled_bowra) <- labelled_bowra$id


labelled_serf <- read.csv(getDataPath("Chapter2_SoundscapeTemporalAssessment", "DiscriminantAnalysis", "wavelet_serf_0_labelled.csv"))

unclass <- filter(labelled_serf, classID == "")

# sample_size <- ceiling(nrow(labelled)*0.04)
# 
# sample(rownames(unclass), size = sample_size, replace = F)

labelled_bowra <- separate(labelled_bowra, col = id, into = c("point", "index", "number", "what"), remove = F)

labelled_bowra$index <- as.factor(labelled_bowra$index)
labelled_serf$index <- as.factor(labelled_serf$index)

labelled_bowra <- select(labelled_bowra, classID, everything(), -c(id, number, what, point))
labelled_serf <- select(labelled_serf, classID, everything(), -c(id, number, what, location))


labelled_bowra_complete <- dplyr::filter(labelled_bowra, classID != "") %>% 
  droplevels(.$classID) %>%
  droplevels(.$index) %>% 
  droplevels(.$component) %>% 
  droplevels(.$bio)

labelled_serf_complete <- dplyr::filter(labelled_serf, classID != "") %>% 
  droplevels(.$classID) %>%
  droplevels(.$index) %>% 
  droplevels(.$component) %>% 
  droplevels(.$bio)

bowra_component <- select(labelled_bowra_complete, -c(classID, bio)) %>% 
  droplevels(.)
serf_component <- select(labelled_serf_complete, -c(classID, bio)) %>% 
  droplevels(.)


bowra_class <- select(labelled_bowra_complete, -c(component, bio)) %>% 
  droplevels(.)
serf_class <- select(labelled_serf_complete, -c(component, bio)) %>% 
  droplevels(.)

bowra_bio <- select(labelled_bowra_complete, -c(classID, component)) %>% 
  droplevels(.)
serf_bio <- select(labelled_serf_complete, -c(classID, component)) %>% 
  droplevels(.)

# train_index <- sample(1:nrow(df_bio), 0.8 * nrow(df_bio))
# test_index <- setdiff(1:nrow(df_bio), train_index)
# 
# 
# train <- df_bio[train_index,]%>% 
#   droplevels(.$bio) %>% 
#   droplevels(.$index)
# 
# 
# test <- df_bio[test_index,]%>% 
#   droplevels(.$bio) %>% 
#   droplevels(.$index)


set.seed(123)

ct <- ctree(bio ~ ., data=serf_bio, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))

pClassId <- predict(ct)

# check predicted classes against original class labels

table(serf_bio$bio, pClassId)

#accuracy

(sum(bowra_bio$bio==pClassId)) / nrow(bowra_bio)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(bio ~ ., data = serf_bio, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

prediction <- predict(ct, newdata = serf_bio)


(sum(test$bio==prediction)) / nrow(test)


library(party)

####

ct <- ctree(classID ~ ., data=serf_class, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))

pClassId <- predict(ct)

# check predicted classes against original class labels

table(serf_class$classID, pClassId)

# accuracy

(sum(serf_class$classID==pClassId)) / nrow(serf_class)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(classID ~ ., data = serf_class, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

predict(rf, newdata = serf_class)

####

ct <- ctree(bio ~ ., data=df_bio, controls = ctree_control(minsplit=10, minbucket=40, maxdepth=5))

#predict(ct, labelled_complete)

pClassId <- predict(ct)

# check predicted classes against original class labels

table(df_bio$bio, pClassId)

# accuracy

(sum(df_bio$bio==pClassId)) / nrow(df_bio)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(bio ~ ., data = df_bio, importance = T, proximity = T)



print(rf)

varImpPlot(rf)


df_biophony <- filter(labelled_complete, component == "biophony") %>% 
  select(everything(), -c(component, bio)) %>% 
  droplevels(.)

####

ct <- ctree(classID ~ ., data=df_biophony, controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5))

pClassId <- predict(ct)

# check predicted classes against original class labels

table(df_biophony$classID, pClassId)

# accuracy

(sum(df_biophony$classID==pClassId)) / nrow(df_biophony)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(classID ~ ., data = df_biophony, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

speccurve <- select(labelled_serf_complete, batch, classID) %>% 
  count(., classID, batch) %>% 
  pivot_wider(., names_from = classID, values_from = n, values_fill = 0)

accum <- specaccum(speccurve, "collector")
accum
summary(accum)
plot(accum, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")