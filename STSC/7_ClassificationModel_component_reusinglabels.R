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


# sample_size <- ceiling(nrow(model_data)*0.30)
# 
# sample <- as.data.frame(sample(rownames(model_data), size = sample_size, replace = F)) %>% 
#   write.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_LabelSample.csv", sep = "")))

labelled <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(train_label, "_wavelet_labelled_balanced.csv", sep = "")))

rownames(labelled) <- labelled$id

unclass <- filter(labelled, classID == "")

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

write.csv(new_data, getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_component_RFlabels.csv", sep = "")), row.names = F)


