library(tidyverse)
library(randomForest)
library(vegan)



rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"

train_label <- "Bowraoct"


new_data <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "wavelet_0_labelled.csv", sep = ""))) %>% 
  rename(., id = X) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix)

new_labels <- filter(new_data, component != "")

head(new_labels)
# sample_size <- ceiling(nrow(model_data)*0.30)
# 
# sample <- as.data.frame(sample(rownames(model_data), size = sample_size, replace = F)) %>% 
#   write.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_LabelSample.csv", sep = "")))

labelled <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(train_label, "_component_RFlabels.csv", sep = ""))) %>% filter(., component_model == component) %>% 
  rbind(labelled, new_labels)

head(labelled)

rownames(labelled) <- labelled$id

model_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., component, index, everything(), -c("point", "number", "what", "classID", "id")) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  filter(., component != "") %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

model_data$index <- as.factor(model_data$index)

plot_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>%
  select(., point, index, number, component, what, id, classID, everything()) %>% 
  mutate_at(vars(10:ncol(.)), na.roughfix) %>% 
  filter(., component != "")  %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

plot_data$point <- as.factor(plot_data$point)
plot_data$index <- as.factor(plot_data$index)

plot(plot_data$component, plot_data$index)
plot(plot_data$point, plot_data$component)

new_data <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_component_RFlabels.csv", sep = ""))) %>% 
  #rename(., id = X) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix)


rownames(new_data) <- new_data$id

new_train_data <- filter(new_data, classID != "") 


#Splitting the data into training (60%) and testing (40%)

set.seed(123)

train <- model_data %>%
  droplevels(.$component) %>%
  droplevels(.$index)

rf <- randomForest(component ~ ., data = train, importance = T, proximity = T, )

print(rf)

varImpPlot(rf)

importance <- as.data.frame(importance(rf)) %>% 
  filter(., MeanDecreaseGini != 0) %>% 
  row.names(.)

model_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., component, index, all_of(importance)) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  filter(., component != "") %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

new_train_data <- separate(new_train_data, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., component, index, all_of(importance)) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  filter(., component != "") %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

model_data <- rbind(model_data, new_train_data)

model_data$index <- as.factor(model_data$index)

set.seed(123)

train_index <- sample(1:nrow(model_data), 0.6 * nrow(model_data))
test_index <- setdiff(1:nrow(model_data), train_index)


train <- model_data[train_index,] %>%
  droplevels(.$component) %>%
  droplevels(.$index)


test <- model_data[test_index,]%>%
  droplevels(.$component) %>%
  droplevels(.$index)

rf <- randomForest(component ~ ., data = train, importance = T, proximity = T, )

print(rf)

varImpPlot(rf)

classifier <- separate(new_data, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., component, index, all_of(importance)) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  #filter(., component == "") %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

classifier$index <- as.factor(classifier$index)

label_model <- predict(rf, newdata = classifier)

new_data$component_model <- label_model

new_data <- select(new_data, id, classID, component, component_model, everything())

table(new_data$component, label_model)


(sum(new_data$component==new_data$component_model)) / nrow(model_data)

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

rf <- randomForest(component ~ ., data = train, importance = T, proximity = T, mtry = mtry)

print(rf)

varImpPlot(rf)

classifier <- select(model_data, component, all_of(importance)) %>% 
  droplevels(.)

classifier$index <- as.factor(classifier$index)

prediction <- predict(rf, newdata = classifier)


table(classifier$component, prediction)


(sum(classifier$component==prediction)) / nrow(classifier)

heatmap(rf$proximity)

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


