library(tidyverse)
library(randomForest)

set.seed(123)

#excel formula: =IF(E2="bird", "biophony", IF(E2="insect", "biophony", IF(E2="wind", "geophony", IF(E2="silence", "silence", IF(E2="anthrophony", "anthrophony", "NA")))))#

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"

labelled <- read.csv(getDataPath("STSC", "Test", paste(data, "wavelet.csv", sep = "")))

rownames(labelled) <- labelled$id

model_data <- select(labelled, component, everything(), -c("id", "class")) %>% 
  filter(., component != "NA") %>% 
  droplevels(.)

#Checking balance
# plot_data <- select(labelled, point, index_name, number, component,  what, id, classID, everything()) %>% 
#   mutate_at(vars(10:ncol(.)), na.roughfix) %>% 
#   filter(., component != "")  %>% 
#   droplevels(.)
# 
# plot_data$point <- as.factor(plot_data$point)
# plot_data$index <- as.factor(plot_data$index)
# 
# plot(plot_data$component, plot_data$index)
# plot(plot_data$point, plot_data$component)

#Splitting into train and test data
train_index <- sample(1:nrow(model_data), 0.6 * nrow(model_data)) #- nrow(train))

train <- rbind(model_data[train_index,]) %>%
droplevels(.)


test_index <- setdiff(1:nrow(model_data), train_index)

test <- model_data[test_index,]%>%
  droplevels(.)

rf <- randomForest(component ~ ., data = train, importance = T, proximity = T)

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

train <- select(train, component, all_of(importance)) %>%
  droplevels(.)

test <- select(test, component, all_of(importance)) %>%
  droplevels(.)

rf <- randomForest(component ~ ., data = train, importance = T, proximity = T, mtry = best.m)

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)


table(test$component, prediction)


(sum(test$component==prediction)) / nrow(test)


classifier <- select(labelled, component, all_of(importance)) %>% 
  droplevels(.)


# classifier$index <- as.factor(classifier$index)


label_model <- as.data.frame(predict(rf, newdata = classifier))

rownames(label_model) <- labelled$id

label_model <- rename(label_model, "RFcomponent" = `predict(rf, newdata = classifier)`)

labelled <- cbind(labelled, label_model) %>% 
  select(., id, class, component, RFcomponent, everything())

confusion_matrix <- table(labelled$component, labelled$RFcomponent)

(sum(labelled$component==labelled$RFcomponent)) / nrow(labelled)

write.csv(labelled, getDataPath("STSC", "Test", paste(data, "_component_RFlabels.csv", sep = "")), row.names = F)
