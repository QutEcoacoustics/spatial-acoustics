library(tidyverse)
library(randomForest)
library(vegan)



rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "SERF"


labelled <- read.csv(getDataPath("Chapter2_SoundscapeTemporalAssessment", "DiscriminantAnalysis", paste(data, "_component_RFlabels.csv", sep = "")))
  
unclass <- filter(labelled, component != "biophony")
rownames(unclass) <- unclass$id

biophony <- filter(labelled, component_model == "biophony") %>%
  mutate_at(vars(7:ncol(.)), na.roughfix)

rownames(biophony) <- biophony$id

summary(biophony)

# labels <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", "wavelet_bow_0_labelled_simple.csv")) %>% 
#   select(1:4)
# 
# new_df <- merge(x = df, y = labels, all.x = T, all.y = F ) %>% 
#   select(., id, classID, component, bio, everything()) %>% 
#   write.csv(., getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "wavelet_labelled.csv", sep = "_")), row.names = F)

#After inspecting motifs, load the df with labelled data

# sample_size <- ceiling(nrow(labelled)*0.30)
# 
# sample(rownames(unclass), size = sample_size, replace = F)

model_data <- separate(biophony, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., classID, index, everything(), -c("point", "id", "number", "what", "component")) %>% 
  filter(., classID != "" & classID != "silence" & classID != "wind") %>%
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  droplevels(.$index) %>% 
  droplevels(.$classID)

model_data$index <- as.factor(model_data$index)

plot_data <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F) %>%
  #select(., point, index, number, component, bio, what, id, batch, classID, columns_to_keep) %>% 
  #filter(., classID != "" & classID != "silence" & classID != "wind")  %>% 
  droplevels(.$index) %>% 
  droplevels(.$classID)

plot_data$point <- as.factor(plot_data$point)
plot_data$index <- as.factor(plot_data$index)

plot(plot_data$classID, plot_data$index)
plot(plot_data$point, plot_data$classID)


#Splitting the data into training (60%) and testing (40%)

set.seed(123)

train_index <- sample(1:nrow(model_data), 0.6 * nrow(model_data))
test_index <- setdiff(1:nrow(model_data), train_index)


train <- model_data[train_index,]%>%
  droplevels(.$classID) %>%
  droplevels(.$index)


test <- model_data[test_index,]%>%
  droplevels(.$classID) %>%
  droplevels(.$index)

rf <- randomForest(classID ~ ., data = train, importance = T, proximity = T, )

print(rf)

varImpPlot(rf)

importance <- as.data.frame(importance(rf)) %>% 
  filter(., MeanDecreaseGini != 0) %>% 
  row.names(.)


model_data <- select(model_data, classID, index, all_of(importance)) %>%  
  mutate_at(vars(3:ncol(.)), na.roughfix) %>% 
  filter(., classID != "") %>% 
  droplevels(.$index) %>% 
  droplevels(.$classID)

set.seed(123)

train_index <- sample(1:nrow(model_data), 0.6 * nrow(model_data))
test_index <- setdiff(1:nrow(model_data), train_index)


train <- model_data[train_index,]%>%
  droplevels(.$classID) %>%
  droplevels(.$index)


test <- model_data[test_index,]%>%
  droplevels(.$classID) %>%
  droplevels(.$index)

rf <- randomForest(classID ~ ., data = train, importance = T, proximity = T, )

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)


table(test$classID, prediction)


(sum(test$classID==prediction)) / nrow(test)

classifier <- separate(biophony, col = id, into = c("point", "index", "number", "what"), remove = F) %>% 
  select(., classID, index, all_of(importance)) %>%
  droplevels(.$index) %>% 
  droplevels(.$classID)

classifier$index <- as.factor(classifier$index)

label_model <- predict(rf, newdata = classifier)

biophony$class_model <- label_model

labelled <- select(biophony, id, classID, component, component_model, class_model, everything())

write.csv(labelled, getDataPath("Chapter2_SoundscapeTemporalAssessment", "DiscriminantAnalysis", paste(data, "_class_RFlabels.csv", sep = "")), row.names = F)


