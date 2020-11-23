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
library(dtwclust)
library(purrr)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraoct"


labelled <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "_wavelet_labelled.csv", sep = "")))


# labels <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", "wavelet_bow_0_labelled_simple.csv")) %>% 
#   select(1:4)
# 
# new_df <- merge(x = df, y = labels, all.x = T, all.y = F ) %>% 
#   select(., id, classID, component, bio, everything()) %>% 
#   write.csv(., getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "wavelet_labelled.csv", sep = "_")), row.names = F)

#After inspecting motifs, load the df with labelled data


rownames(labelled) <- labelled$id



unclass <- filter(labelled, classID == "")

sample_size <- ceiling(nrow(labelled)*0.30)

sample(rownames(unclass), size = sample_size, replace = F)

labelled <- separate(labelled, col = id, into = c("point", "index", "number", "what"), remove = F)

labelled$index <- as.factor(labelled$index)

labelled_complete <- select(labelled, classID, everything(), -c(id, number, what, point, batch)) %>% 
  na.roughfix(labelled)


labelled_complete <- dplyr::filter(labelled_complete, classID != "") %>% 
  droplevels(.$classID) %>%
  droplevels(.$index) %>% 
  droplevels(.$component) %>% 
  droplevels(.$bio)

summary(labelled_complete)

component <- select(labelled_complete, -c(classID, bio)) %>% 
  droplevels(.)


class <- filter(labelled_complete, bio == "bio") %>% 
  select(., -c(component, bio)) %>%
  droplevels(.)

bio <- select(labelled_complete, -c(classID, component)) %>% 
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

ct <- ctree(bio ~ ., data=bio, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))

pClassId <- predict(ct)

# check predicted classes against original class labels

table(bio$bio, pClassId)

#accuracy

(sum(bio$bio==pClassId)) / nrow(bio)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(bio ~ ., data = bio, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

prediction <- predict(ct, newdata = bio)


(sum(test$bio==prediction)) / nrow(test)


library(party)

####

ct <- ctree(classID ~ ., data=class, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))

pClassId <- predict(ct)

# check predicted classes against original class labels

table(class$classID, pClassId)

# accuracy

(sum(class$classID==pClassId)) / nrow(class)



plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))



rf <- randomForest(classID ~ ., data = class, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

predict(rf, newdata = serf_class)


speccurve <- filter(labelled, batch != "") %>% 
  select(., batch, classID) %>% 
  count(., classID, batch) %>% 
  pivot_wider(., names_from = classID, values_from = n, values_fill = 0)

accum <- specaccum(speccurve, "collector")
accum
summary(accum)
plot(accum, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

