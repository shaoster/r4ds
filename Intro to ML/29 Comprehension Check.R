library(dslabs)
library(dplyr)
library(lubridate)
library(purrr)
library(caret)
library(tidyverse)
library(plotly)

data(heights)

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

#ls = levels("Male", "Female")
runsim <- function(k) {
    knn_fit <- knn3(sex ~ height, data=train_set, k=k)
    y_hat_knn <- predict(knn_fit, test_set, type = "class")
    F_meas(data=y_hat_knn, reference=test_set$sex)
}

k <- seq(1,101,3)
accuracies <- sapply(k, runsim)
best_index <- which.max(accuracies)
accuracies[best_index]
k[best_index]


#2
data(tissue_gene_expression)
set.seed(1)
test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
test_x <- tissue_gene_expression$x[test_index,]
test_y <- tissue_gene_expression$y[test_index]
test_set <- data.frame(test_x, y = test_y)
train_x <- tissue_gene_expression$x[-test_index,]
train_y <- tissue_gene_expression$y[-test_index]
train_set <- data.frame(train_x, y =train_y)

runsim <- function(k) {
    knn_fit <- knn3(y ~ ., data=train_set, k=k)
    y_hat_knn <- predict(knn_fit, test_set, type = "class")
    confusionMatrix(data=y_hat_knn, reference=test_set$y)$overall["Accuracy"]
}

k <- seq(1,11,2)
accuracies <- sapply(k, runsim)
best_index <- which.max(accuracies)
accuracies[best_index]
k[best_index]

    knn_fit <- knn3(y ~ ., data=train_set, k=3)
    y_hat_knn <- predict(knn_fit, test_set, type = "class")
    confusionMatrix(data=y_hat_knn, reference=test_set$y)$overall["Accuracy"]
