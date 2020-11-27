library(tidyverse)
library(randomForest)
library(vegan)
library(splitstackshape)
library(ggplot2)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "SERF"

chapter <- "Chapter2_SoundscapeTemporalAssessment"

#After inspecting motifs, load the df with labelled data

# sample_size <- ceiling(nrow(labelled)*0.30)
# 
# sample(rownames(unclass), size = sample_size, replace = F)



# labelled <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_component_RFlabels1.csv", sep = "")))
# 
# 
# motifs <- read.csv(getDataPath("STSC", "Results", data, paste(data, "motif_complete.csv", sep = ""))) %>% 
#   select(., id, time) %>% 
#   distinct(id, .keep_all = T)
# 
# test <- left_join(x = labelled, y = motifs, keep = F) %>% 
#   select(id, classID, component, component_model1, time, everything()) %>% 
#   write.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", "test.csv"), row.names = F)

#1 - DF with labels created by the component model and select only biophony

labelled <- read.csv(getDataPath(chapter, "DiscriminantAnalysis", paste(data, "_component_RFlabels.csv", sep = ""))) %>% 
  mutate_at(vars(5:ncol(.)), na.roughfix) %>% 
  filter(., component_model == "biophony" & component != "silence" & component != "geophony") %>%
  droplevels(.)

rownames(labelled) <- labelled$id

#labelled <- mutate_at(labelled, vars(6:ncol(labelled)), na.roughfix)

model_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., classID, index, everything(), -c("point", "id", "number", "what", "component", "component_model")) %>% 
  droplevels(.)

model_data$index <- as.factor(model_data$index)


#2 - Separate labelled for training and testing the model - here we need to ensure the training data has all the predictors otherwise the model is unable to use them in the testing data

train <- stratified(model_data, group = c("index", "classID"), size = 12, replace = F) %>% 
  droplevels(.)

# train_index <- sample(1:nrow(model_data), 0.2 * nrow(model_data)) #- nrow(train))

# train <- rbind(model_data[train_index,], train) %>%
  # droplevels(.)


# test_index <- setdiff(1:nrow(model_data), train_index)
# 
# 
# test <- model_data[test_index,]%>%
#   droplevels(.)
  
test_index <- sample(1:nrow(model_data), 0.1 * nrow(model_data))
test <- model_data[test_index,]%>%
    droplevels(.)
  

#3 - Build rough model using all the variables




# model_data$index <- as.factor(model_data$index)
# model_data$time <- as.factor(model_data$time)
# 
# summary(model_data)
# 
# 
# 
# summary(labelled_test)
# 
# labelled_test$index <- as.factor(labelled_test$index)
# labelled_test$time <- as.factor(labelled_test$time)
# 
# # plot_data <- separate(labelled_test, col = id, into = c("point", "index", "number", "what"), remove = F) %>%
# #   #select(., point, index, number, component, bio, what, id, batch, classID, columns_to_keep) %>% 
# #   #filter(., classID != "" & classID != "silence" & classID != "wind")  %>% 
# #   droplevels(.$index) %>% 
# #   droplevels(.$classID) %>% 
# #   droplevels(.$time)
# 
# plot(labelled_test$classID, labelled_test$index)
# plot(labelled_test$classID, labelled_test$time)
# 
# 
# #Splitting the data into training (60%) and testing (40%)
# 
# 
# 
# set.seed(123)
# 
# train_index <- sample(1:nrow(model_data), 0.1 * nrow(model_data))
# 
# test_index <- setdiff(1:nrow(labelled_test), train_index)
# 
# 
# train <- rbind(model_data[train_index,], labelled_time)%>%
#   droplevels(.$classID) %>%
#   droplevels(.$index) %>%
#   droplevels(.$time)
# 
# 
# test <- labelled_test[test_index,]%>%
#   droplevels(.$classID) %>%
#   droplevels(.$index) %>% 
#   droplevels(.$time)
  

rf <- randomForest(classID ~ ., data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)

table(test$classID, prediction)

(sum(test$classID==prediction)) / nrow(test)

#4 - Optimising the model

importance <- as.data.frame(importance(rf)) %>% 
  filter(., MeanDecreaseAccuracy >= 0) %>% 
  row.names(.)

model_data <- select(model_data, classID, all_of(importance)) %>% 
  droplevels(.)

floor(sqrt(ncol(model_data) - 1))

mtry <- tuneRF(model_data[-1],model_data$classID, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


#2 - Separate labelled for training and testing the model - here we need to ensure the training data has all the predictors otherwise the model is unable to use them in the testing data


train <- select(train, classID, all_of(importance)) %>% 
  droplevels(.)

test <- select(test, classID, all_of(importance)) %>% 
  droplevels(.)


set.seed(123)


rf <- randomForest(classID ~ ., data = train, importance = T, proximity = T, keep.forest = T, mtry = best.m)

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)


table(test$classID, prediction)

(sum(test$classID==prediction)) / nrow(test)

classifier <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., classID, all_of(importance)) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  #filter(., component == "") %>% 
  droplevels(.)

classifier$index <- as.factor(classifier$index)

label_model <- predict(rf, newdata = classifier)



labelled$class_model <- label_model

labelled <- select(labelled, id, classID, component, component_model, class_model, everything())

write.csv(labelled, getDataPath(chapter, "DiscriminantAnalysis", paste(data, "_class_RFlabels.csv", sep = "")), row.names = T)


