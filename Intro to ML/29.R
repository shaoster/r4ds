library(dslabs)
library(tidyverse)
library(plotly)
library(caret)

data("mnist_27")

plot <- mnist_27$test %>%
    ggplot(aes(x_1, x_2, color = y)) +
    geom_point()
ggplotly(plot)

knn_fit <- knn3(y ~ x_1 + x_2, data=mnist_27$train)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
cm <- confusionMatrix(y_hat_knn, mnist_27$test$y)

#overtraining with k=1
knn_fit_1 <- knn3(y ~ x_1 + x_2, data=mnist_27$train, k=1)

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
cm_1 <- confusionMatrix(y_hat_knn_1, mnist_27$train$y)
y_hat_knn_2 <- predict(knn_fit_1, mnist_27$test, type = "class")
cm_2 <- confusionMatrix(y_hat_knn_2, mnist_27$test$y)

#oversmoothing with large k
knn_fit_401 <- knn3(y ~ x_1 + x_2, data=mnist_27$train, k=401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
cm <- confusionMatrix(y_hat_knn_401, mnist_27$test$y)
