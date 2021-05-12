library(caret)
library(repr)
library(data.table)
library(TTR)
library(forecast)
library(lubridate)
library(wavelets)
library(vegan)
library(dtwclust)
library(purrr)

library(randomForest)
library(tidyverse)
library(party)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"

labelled <- read.csv(getDataPath("STSC", "Test", paste(data, "wavelet.csv", sep = "")))

unlabelled <- filter(labelled, class == "NA")

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




train_index <- sample(1:nrow(class), 0.6 * nrow(class))
test_index <- setdiff(1:nrow(class), train_index)


train <- class[train_index,]%>%
  droplevels(.$classID) %>%
  droplevels(.$index)


test <- class[test_index,]%>%
  droplevels(.$classID) %>%
  droplevels(.$index)


set.seed(123)

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



rf <- randomForest(classID ~ ., data = train, importance = T, proximity = T, na)

print(rf)

varImpPlot(rf)

prediction <- predict(rf, newdata = test)

table(test$classID, prediction)


(sum(test$classID==prediction)) / nrow(test)


library(party)

####

ct <- ctree(classID ~ ., data=class, controls = ctree_control(minsplit=30, minbucket=40, maxdepth=5))

pClassId <- predict(ct)

# check predicted classes against original class labels

table(class$classID, pClassId)

# accuracy

(sum(test$component==prediction)) / nrow(test)



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

