library(tidyverse)
library(randomForest)
library(vegan)



rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "SERF"

chapter <- "Chapter2_SoundscapeTemporalAssessment"


labelled <- read.csv(getDataPath(chapter, "DiscriminantAnalysis", paste(data, "_component_RFlabels.csv", sep = ""))) %>% 
  mutate_at(vars(6:ncol(.)), na.roughfix)


# labels <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", "wavelet_bow_0_labelled_simple.csv")) %>% 
#   select(1:4)
# 
# new_df <- merge(x = df, y = labels, all.x = T, all.y = F ) %>% 
#   select(., id, classID, component, bio, everything()) %>% 
#   write.csv(., getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "wavelet_labelled.csv", sep = "_")), row.names = F)

#After inspecting motifs, load the df with labelled data


rownames(labelled) <- labelled$id

unclass <- filter(labelled, classID == "")

# sample_size <- ceiling(nrow(labelled)*0.30)
# 
# sample(rownames(unclass), size = sample_size, replace = F)

model_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., component, index, everything(), -c("point", "number", "what", "classID", "id", "component_model")) %>% 
  filter(., component != "") %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

model_data$index <- as.factor(model_data$index)



plot_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>%
  select(., point, index, number, component,  what, id, classID, everything()) %>% 
  mutate_at(vars(10:ncol(.)), na.roughfix) %>% 
  filter(., component != "")  %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

plot_data$point <- as.factor(plot_data$point)
plot_data$index <- as.factor(plot_data$index)

plot(plot_data$component, plot_data$index)
plot(plot_data$point, plot_data$component)


#Splitting the data into training (60%) and testing (40%)

set.seed(123)

train <- stratified(model_data, group = c("index", "component"), size = 0.2, replace = F) %>% 
  droplevels(.)

#train_index <- sample(1:nrow(model_data), 0.2 * nrow(model_data))

# train <- rbind(model_data[train_index,], train)%>%
#   droplevels(.)

#train <- model_data[train_index,]

#test_index <- setdiff(1:nrow(model_data), train_index)

test_index <- sample(1:nrow(model_data), 0.1 * nrow(model_data))
test <- model_data[test_index,]%>%
  droplevels(.)

rf <- randomForest(component ~ ., data = train, importance = T, proximity = T, )

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)


table(test$component, prediction)


(sum(test$component==prediction)) / nrow(test)

#Optimising

importance <- as.data.frame(importance(rf)) %>%
   filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)
# 
model_data <- select(model_data, component, all_of(importance)) %>%
  droplevels(.)

floor(sqrt(ncol(model_data) - 1))

mtry <- tuneRF(model_data[-1],model_data$component, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#Optimising

model_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>%
  select(., component, all_of(importance)) %>%
  filter(., component != "") %>%
  droplevels(.$index) %>%
  droplevels(.$component)
# 
# set.seed(123)
# 
# 
train <- select(train, component, all_of(importance)) %>%
  droplevels(.)


test <- select(test, component, all_of(importance)) %>%
  droplevels(.)

rf <- randomForest(component ~ ., data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)



prediction <- predict(rf, newdata = test)


table(test$component, prediction)


(sum(test$component==prediction)) / nrow(test)


classifier <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., component, all_of(importance)) %>% 
  #filter(., component == "") %>% 
  droplevels(.)


classifier$index <- as.factor(classifier$index)


label_model <- as.data.frame(predict(rf, newdata = classifier, type = "prob"))

rownames(label_model) <- labelled$id

label_model$manual <- labelled$component

label_model$model <- labelled$component_model

library(ggplot2)

#pivot_longer(label_model, cols = 4:5, names_to = "labeller", values_to = "label") %>% 
ggplot(label_model, aes(y = biophony, x = geophony, colour = manual)) +
    geom_point() +
  ggsave(getDataPath(chapter, "DiscriminantAnalysis", paste(data, "_ProbPlot.png")))

label_model <- predict(rf, newdata = classifier)


labelled$component_model <- label_model


labelled <- select(labelled, id, classID, component, component_model, everything())

confusion_matrix <- table(labelled$component, labelled$component_model)

(sum(labelled$component==labelled$component_model)) / nrow(labelled)

#write.csv(labelled, getDataPath(chapter, "DiscriminantAnalysis", paste(data, "_component_RFlabels.csv", sep = "")), row.names = F)
