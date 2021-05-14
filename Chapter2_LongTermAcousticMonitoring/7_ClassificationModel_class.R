library(tidyverse)
library(randomForest)

set.seed(123)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"


labelled <- read.csv(getDataPath("STSC", "Test", paste(data, "_component_RFlabels.csv", sep = "")))
rownames(labelled) <- labelled$id

# labels <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", "wavelet_bow_0_labelled_simple.csv")) %>% 
#   select(1:4)
# 
# new_df <- merge(x = df, y = labels, all.x = T, all.y = F ) %>% 
#   select(., id, classID, component, bio, everything()) %>% 
#   write.csv(., getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "wavelet_labelled.csv", sep = "_")), row.names = F)

#After inspecting motifs, load the df with labelled data



biophony <- filter(labelled, RFcomponent == "biophony") %>% 
  select(., everything(), -c(id, component, RFcomponent)) %>% 
  droplevels(.)

additional_sampling <- sample(rownames(biophony), size = ceiling(nrow(labelled)*0.30), replace = F)

# labelled <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F)

# biophony$index <- as.factor(labelled$index)

# labelled_complete <- select(labelled, classID, everything(), -c(id, number, what, point, batch)) %>% 
#   na.roughfix(labelled)

model_data <- filter(biophony, class == "insect" | class == "bird") %>% 
  droplevels(.)

train_index <- sample(1:nrow(model_data), 0.6 * nrow(model_data))
test_index <- setdiff(1:nrow(model_data), train_index)


train <- model_data[train_index,]%>%
  droplevels(.)


test <- model_data[test_index,]%>%
  droplevels(.)




# ct <- ctree(bio ~ ., data=bio, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))
# 
# pClassId <- predict(ct)
# 
# # check predicted classes against original class labels
# 
# table(bio$bio, pClassId)
# 
# #accuracy
# 
# (sum(bio$bio==pClassId)) / nrow(bio)
# 
# 
# 
# plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(class ~ ., data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)

table(test$class, prediction)


(sum(test$class==prediction)) / nrow(test)

#Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)
# 
model_data <- select(model_data, class, all_of(importance)) %>%
  droplevels(.)

floor(sqrt(ncol(model_data) - 1))

mtry <- tuneRF(model_data[-1],model_data$class, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#Optimising

train <- select(train, class, all_of(importance)) %>%
  droplevels(.)

test <- select(test, class, all_of(importance)) %>%
  droplevels(.)

rf <- randomForest(class ~ ., data = train, importance = T, proximity = T, mtry = best.m)

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)


table(test$class, prediction)


(sum(test$class==prediction)) / nrow(test)


classifier <- select(biophony, class, all_of(importance)) %>% 
  droplevels(.)


# classifier$index <- as.factor(classifier$index)


label_model <- as.data.frame(predict(rf, newdata = classifier))

rownames(label_model) <- biophony$id

label_model <- rename(label_model, "RFclass" = `predict(rf, newdata = classifier)`)

final_df <- cbind(biophony, label_model) %>% 
  select(., class, RFclass, everything())

confusion_matrix <- table(final_df$class, final_df$RFclass)

write.csv(final_df, getDataPath("STSC", "Test", paste(data, "_class_RFlabels.csv", sep = "")), row.names = T)
