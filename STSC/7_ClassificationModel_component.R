library(tidyverse)
library(randomForest)
library(vegan)



rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraoct"


labelled <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_wavelet_labelled_balanced.csv", sep = "")))


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
  select(., component, index, everything(), -c("point", "number", "what", "classID", "bio", "batch", "id")) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  filter(., component != "") %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

model_data$index <- as.factor(model_data$index)



plot_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>%
  select(., point, index, number, component, bio, what, id, batch, classID, everything()) %>% 
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

train_index <- sample(1:nrow(model_data), 0.6 * nrow(model_data))
test_index <- setdiff(1:nrow(model_data), train_index)


train <- model_data[train_index,]%>%
  droplevels(.$component) %>%
  droplevels(.$index)


test <- model_data[test_index,]%>%
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

model_data$index <- as.factor(model_data$index)

set.seed(123)

train_index <- sample(1:nrow(model_data), 0.6 * nrow(model_data))
test_index <- setdiff(1:nrow(model_data), train_index)


train <- model_data[train_index,]%>%
  droplevels(.$component) %>%
  droplevels(.$index)


test <- model_data[test_index,]%>%
  droplevels(.$component) %>%
  droplevels(.$index)

rf <- randomForest(component ~ ., data = train, importance = T, proximity = T, )

print(rf)

varImpPlot(rf)



prediction <- predict(rf, newdata = test)


table(test$component, prediction)


(sum(test$component==prediction)) / nrow(test)


classifier <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., component, index, all_of(importance)) %>% 
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  #filter(., component == "") %>% 
  droplevels(.$index) %>% 
  droplevels(.$component)

classifier$index <- as.factor(classifier$index)

label_model <- predict(rf, newdata = classifier)

labelled$component_model <- label_model

labelled <- select(labelled, id, batch, classID, component, bio, component_model, everything())

write.csv(labelled, getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_component_RFlabels.csv", sep = "")), row.names = F)


