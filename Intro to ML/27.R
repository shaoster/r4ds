#27.3
library(dslabs)
library(tidyverse)

library(ggthemes)
library(plotly)
mnist <- read_mnist()
y <- mnist$train$labels

glimpse(mnist)

glimpse(mnist$train)

library(caret)
data(heights)
y <- heights$sex
x <- heights$height

set.seed(209)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index,]
train_set <- heights[-test_index,]

View(test_set)
levels(test_set$sex)

y_hat <- sample(c("Male","Female"), length(test_index), replace = TRUE) %>%
    factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

heights %>%
    group_by(sex) %>%
    summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

cutoff <- seq(61,70)
accuracy <- map_vec(cutoff, function(x){
    y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
    mean(y_hat == train_set$sex)
})

accuracy_by_cutoff <- data.frame(cutoff = cutoff, accuracy = accuracy)
View(accuracy_by_cutoff)

max(accuracy_by_cutoff$accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
    factor(levels = levels(y))

mean(y_hat == test_set$sex)

heights %>%
    group_by(sex) %>%
    summarize(length(height))

table(predicted = y_hat, actual = test_set$sex)

test_set %>%
    mutate(y_hat = y_hat) %>%
    group_by(sex) %>%
    summarize(accuracy = mean(y_hat == sex))

 cm <- confusionMatrix(data = y_hat, reference = test_set$sex)

 F_1 <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
        factor(levels = levels(test_set$sex))
    F_meas(data = y_hat, reference = factor(train_set$sex))
})

F_1_by_cutoff <- data.frame(cutoff = cutoff, F_1 = F_1)
View(F_1_by_cutoff)

plot <- ggplot(
    data = F_1_by_cutoff,
    mapping = aes(x = cutoff, y = F_1)
    ) +
    geom_line() +
    geom_point()

ggplotly(plot)

best_cutoff_2 <- cutoff[which.max(F_1)]

y_hat_2 <- ifelse(test_set$height > best_cutoff_2, "Male", "Female") %>%
    factor(levels = levels(y))

sensitivity(data = y_hat_2, reference = test_set$sex)
specificity(data = y_hat_2, reference = test_set$sex)

library(purrr)
ls <- c("Female", "Male")
fs <- partial(factor, levels = ls)
n <- length(test_set$sex)
probs <- seq(from = 0,to = 1,length.out = 11)
guessing <- map_df(probs, function(p){
    y_hat <- 
        sample(c("Male", "Female"), n, replace = TRUE, prob=c(p,1-p)) %>% fs 
    list(method = "Guessing",
         FPR = 1- specificity(y_hat, test_set$sex),
         TPR = sensitivity(y_hat, test_set$sex)
         )    
})

cutoffs <- c(50, seq(60,75), 80)
height_cutoff <- map_df(cutoffs, function(x){
    y_hat <- 
        ifelse(test_set$height > x, "Male", "Female") %>% fs 
    list(method = "Height cutoff",
         FPR = 1- specificity(y_hat, test_set$sex),
         TPR = sensitivity(y_hat, test_set$sex)
         )    
})

plot <- ggplot(
    data = bind_rows(guessing, height_cutoff),
    mapping = 
        aes(x = FPR, y = TPR, color = method)
    ) +
    geom_line() +
    geom_point() +
    xlab("1-Specificity") +
    ylab("Sensitivity")

ggplotly(plot)

#precision against recall
guessing <- map_df(probs, function(p){
    y_hat <- 
        sample(ls, n, replace = TRUE, prob=c(p,1-p)) %>% fs 
    list(method = "Guessing",
         recall = sensitivity(y_hat, test_set$sex),
         precision = precision(y_hat, test_set$sex)
         )    
})

height_cutoff <- map_df(cutoffs, function(x){
    y_hat <- 
        ifelse(test_set$height > x, "Male", "Female") %>% fs 
    list(method = "Height cutoff",
         recall = sensitivity(y_hat, test_set$sex),
         precision = precision(y_hat, test_set$sex)
         )    
})

plot <- ggplot(
    data = bind_rows(guessing, height_cutoff),
    mapping = 
        aes(x = recall, y = precision, color = method)
    ) +
    geom_line() +
    geom_point() +
    xlab("Sensitivity") +
    ylab("Precision")

ggplotly(plot)

#reverse positive and negative outcomes
ls <- c("Male", "Female")
fs <- partial(factor, levels = ls)
ts <- relevel(test_set$sex, "Male", "Female")

guessing <- map_df(probs, function(p){
    y_hat <- 
        sample(ls, n, replace = TRUE, prob=c(p,1-p)) %>% fs 
    list(method = "Guessing",
         recall = sensitivity(y_hat, ts),
         precision = precision(y_hat, ts)
         )    
})

height_cutoff <- map_df(cutoffs, function(x){
    y_hat <- 
        ifelse(test_set$height > x, "Male", "Female") %>% fs 
    list(method = "Height cutoff",
         recall = sensitivity(y_hat, ts),
         precision = precision(y_hat, ts)
         )    
})

plot <- ggplot(
    data = bind_rows(guessing, height_cutoff),
    mapping = 
        aes(x = recall, y = precision, color = method)
    ) +
    geom_line() +
    geom_point() +
    xlab("Sensitivity") +
    ylab("Precision")

ggplotly(plot)






