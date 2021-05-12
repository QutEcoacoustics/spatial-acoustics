library(vegan)
library(tidyverse)

rm(list = ls())

set.seed(2)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

indices <- read.csv(getDataPath(chapter, "27.01.2021_LabelsIndicesLand.csv"))

summary_stats <- group_by(indices, id) %>% 
  summarise(., mean = mean(index_value), sd = sd(index_value)) %>% 
  left_join(indices, ., by = "id") %>% 
  distinct(., id, .keep_all = T)

#summary_stats <- summary_stats %>%  mutate_at(c(70:77, 84, 85, 87:99), scale)

summary(summary_stats)

model_data <- summary_stats[c(7, 70:77, 84, 85, 87:97)]

#Cluster

set.seed(123)


dist_mat <- dist(model_data, method = "euclidean")

hclust_avg <- eclust(model_data, "hclust", k = 12, hc_metric = "euclidean")
fviz_silhouette(hclust_avg)

plot(hclust_avg)


library(fpc)
library(factoextra)
library(NbClust)


fviz_silhouette(hclust_avg)

cluster.stats(d = dist_mat, clustering = hclust_avg$cluster)

cluster_assign <- mutate(model_data, cluster = hclust_avg$cluster)

#NMDS

set.seed(123)

example_NMDS <- metaMDS(model_data) 
print(example_NMDS)

stressplot(example_NMDS)

plot(example_NMDS)


plot(model_data, type = "n")

#Discriminant Analysis
set.seed(123)

library(caret)
library(MASS)
library(mda)



train_index <- sample(1:nrow(model_data), 0.6 * nrow(model_data))
test_index <- setdiff(1:nrow(model_data), train_index)


train <- model_data[train_index,]%>%
  droplevels(.)


test <- model_data[test_index,]%>%
  droplevels(.)

model <- rda(class_model ~ ., data = train)
print(model)

pred <- predict(test)

plot(model)


