library(tidyverse)
library(caret)
library(repr)
library(data.table)
library(TTR)
library(forecast)
library(lubridate)
library(randomForest)
library(wavelets)



rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

# #df with the motifs and filenames
# 
# cluster_results <- read.csv(getDataPath("STSC", "Results", "matchmotif_bow_complete.csv"))
# 
# 
# #cluster_results <- mutate(cluster_results, name_img_file = paste(cluster_results$id, cluster_results$FileName, sep = "_")) 
# 
# 
# #Randomly selecting motifs to inspect
# 
# 
# 
# # extracting DWT coefficients (with Haar filter)
# ts_data <- select(cluster_results, index_value, position, id) %>%
#   group_by(., id) %>% 
#   mutate(., new_position = order(order(position))) %>% 
#   mutate(., new_time = min(time)) %>% 
#   ungroup(.) %>% 
#   select(., everything(), -position) %>% 
#   pivot_wider(., names_from = new_position, values_from = index_value) %>% 
#   as.data.frame(.)
# 
# 
# rownames(ts_data) <- ts_data$id
# ts_data <- ts_data[,3:length(ts_data)]
# 
# ts_data[is.na.data.frame(ts_data)] <- 0
# 
# sample_size <- ceiling(nrow(ts_data)*0.01)
# 
# sampling <- sample(rownames(ts_data), size = sample_size, replace = F)
# 
# wtData <- NULL
# 
# for (i in 1:nrow(ts_data)) {
#   
#   a <- t(ts_data[i,])
#   
#   wt <- dwt(a, filter="haar", boundary= "periodic")
#   
#   wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
#   
# }
# 
# wtData <- as.data.frame(wtData)
# rownames(wtData) <- rownames(ts_data)

#write.csv(wtData, getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", "wavelet_bow_0.csv"), row.names = T)

#After inspecting motifs, load the df with labelled data

labelled <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", "wavelet_bow_0_labelled_simple.csv"))
rownames(labelled) <- labelled$id

unclass <- filter(labelled, classID == "")

sample_size <- ceiling(nrow(labelled)*0.04)

sample(rownames(unclass), size = sample_size, replace = F)

labelled <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F)


labelled$index <- as.factor(labelled$index)

labelled <- select(labelled, classID, everything(), -c(id, number, what, point))

# plot_data <- pivot_longer(labelled_complete, cols = 3:130, names_to = "wavelet")
# 
# 
# library(ggplot2)
# ggplot(plot_data, aes(x = wavelet, y = value))+
#   geom_line(aes(colour = index)) +
#   facet_wrap(.~classID)


#train and test datasets

#Random sample indexes


#Build X_train, y_train, X_test, y_test


labelled_complete <- dplyr::filter(labelled, classID != "") %>% 
  droplevels(.$classID) %>%
  droplevels(.$index) %>% 
  droplevels(.$component) %>% 
  droplevels(.$bio)

df_component <- select(labelled_complete, -c(classID, bio)) %>% 
  droplevels(.)

df_class <- select(labelled_complete, -c(component, bio)) %>% 
  droplevels(.)

df_bio <- select(labelled_complete, -c(classID, component)) %>% 
  droplevels(.)

train_index <- sample(1:nrow(df_bio), 0.8 * nrow(df_bio))
test_index <- setdiff(1:nrow(df_bio), train_index)


train <- df_bio[train_index,]%>% 
  droplevels(.$bio) %>% 
  droplevels(.$index)


test <- df_bio[test_index,]%>% 
  droplevels(.$bio) %>% 
  droplevels(.$index)


set.seed(123)


library(party)

ct <- ctree(bio ~ ., data=train, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))

pClassId <- predict(ct)

# check predicted classes against original class labels

table(train$bio, pClassId)

#accuracy

(sum(train$bio==pClassId)) / nrow(train)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(bio ~ ., data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)


(sum(test$bio==prediction)) / nrow(test)




library(party)

####

ct <- ctree(classID ~ ., data=df_class, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))

pClassId <- predict(ct)

# check predicted classes against original class labels

table(df_class$classID, pClassId)

# accuracy

(sum(df_class$classID==pClassId)) / nrow(df_class)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(classID ~ ., data = df_class, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

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

