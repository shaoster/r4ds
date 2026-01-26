library(dslabs)
library(caret)
library(tidyverse)
library(plotly)
library(rpart)

fit <- rpart(margin ~ ., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
dev.off()

plot <- polls_2008 %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_step(aes(day, y_hat), col = "red")
ggplotly(plot)

train_rpart <- train( y ~., method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)), data = mnist_27$train)
plot(train_rpart)
dev.off()

cm <- confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)
dev.off()

library(randomForest)
train_rf <- randomForest(y ~., data=mnist_27$train)
cm_rf <- confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)

train_rf_2 <- train(y~., method = "Rborist", tuneGrid = data.frame(predFixed = 2, minNode = c(3,50)), data= mnist_27$train)
cm_rf_2 <- confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)
