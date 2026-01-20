library(dslabs)
library(caret)
library(tidyverse)
library(plotly)

data("mnist_27")
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

cm_glm <- confusionMatrix(data=y_hat_glm, reference = mnist_27$test$y)
cm_knn <- confusionMatrix(data=y_hat_knn, reference = mnist_27$test$y)

getModelInfo("knn")
modelLookup("knn")

plot <- ggplot(train_knn, highlight=TRUE)
ggplotly(plot)

set.seed(2008)
train_knn <- train(y ~., method = "knn", data = mnist_27$train, tuneGrid = data.frame(k=seq(9,67,2)))

plot <- ggplot(train_knn, highlight=TRUE)
ggplotly(plot)

train_knn$bestTune
train_knn$finalModel

cm_test <- confusionMatrix(data =predict(train_knn, mnist_27$test, type = "raw"),reference = mnist_27$test$y)

#10 -fold cross validation
control <- trainControl(method = "cv", number = 10, p = 0.9)
train_knn_cb <- train(y~., method = "knn", data = mnist_27$train, tuneGrid = data.frame(k=seq(9,67,2)), trControl = control)

plot <- ggplot(train_knn_cb, highlight=TRUE)
ggplotly(plot)

names(train_knn$results)

glimpse(mnist_27$true_p)

plot_cond_prob <- function(p_hat = NULL) {
    tmp <- mnist_27$true_p
    if (!is.null(p_hat)) {
        tmp <- tmp %>%
            mutate(p = p_hat)
    }
    plot <- tmp %>% ggplot(aes(x_1,x_2, z=p, fill=p)) +
        geom_raster(show.legend = FALSE) +
        scale_fill_gradientn(colors = c("#F8766D","white","#00BFC4")) +
        stat_contour(breaks = c(0.5), color = "black")
    ggplotly(plot)
}

predict_knn <- predict(train_knn, mnist_27$true_p, type = "prob")
plot_cond_prob(predict_knn[,2])