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


#Bootstrap
library(dslabs)
library(caret)
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

count_digits <- function(sample) {
    count_3 <- sum(sample == 3)
    count_4 <- sum(sample == 4)
    count_7 <- sum(sample == 7)
    c(count_3, count_4, count_7)
}

sum_by_element <- function(a, b) { Map("+", a, b) }

across_sample <- lapply(indexes, count_digits)

totals = Reduce(sum_by_element, across_sample)

#Q3
set.seed(1)
B <- 10^4
M <- replicate(B, {
    y <- rnorm(100,0,1)
    quantile(y, 0.75)
})

mean(M)
std_error <- sd(M) / sqrt(length(M))

set.seed(1)
p <- rnorm(100,0,1)
B <- 10^4

M <- replicate(B, {
    y <- sample(p,100, replace = TRUE)
    quantile(y, 0.75)
})

mean(M)
std_error <- sd(M) / sqrt(length(M))
