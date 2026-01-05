library(dslabs)
library(dplyr)
library(lubridate)
library(purrr)
library(caret)

data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
    filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
    mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass", "online"))  %>%
    select(sex,type)

ls <- c("Female", "Male")
y <- factor(dat$sex, levels = ls)
x <- dat$type
fs <- partial(factor, levels = ls)


inclass <- filter(dat, type == "inclass")
inclass_female <- mean(inclass$sex == "Female")

dat %>% group_by(type, sex) %>% summarize(p = n())
dat %>% group_by(type) %>% summarize(mean(sex == "Female"))

y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>% fs
prec <- precision(data = y_hat, reference = y)
table(y_hat, y)

cm <- confusionMatrix(data = y_hat, reference = y)

sen <- sensitivity(data = y_hat, reference = y)


# Part 2
data(iris)
iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species
ls = levels(y)
set.seed(76)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

for (col_name in colnames(iris[, -c(5)])) {
    min_threshold <- min(iris[col_name])
    max_threshold <- max(iris[col_name])
    cutoffs <- seq(min_threshold, max_threshold, 0.1)
    accuracy <- map_vec(cutoffs, function(x) {
        y_hat <- ifelse(train[col_name] > x, "virginica", "versicolor") %>%
        factor(levels = ls)
        mean(y_hat == train$Species)
    })
    print(col_name)
    print(cutoffs[which.max(accuracy)])
    print(max(accuracy))
}

y_hat <- ifelse(test$Petal.Width > 1.5 & test$Petal.Length > 4.6, "virginica", "versicolor") %>%
        factor(levels = ls)
mean(y_hat == test$Species)

(ggplot(data=iris) + 
    geom_point(mapping = aes(x = iris$Petal.Width, y = iris$Petal.Length, color=iris$Species))) %>%
    ggplotly()

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace = TRUE, prob = c(0.98, 0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace = TRUE, prob = c(0.9,0.1))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace = TRUE, prob = c(0.15,0.85))

ls <- c(1, 0)
y <- factor(disease, levels = ls)
y_hat <- factor(test, levels = ls)
prec <- precision(data = y_hat, reference = y)
table(y_hat, y)
mean(y_hat == 1)
mean(disease[test==1])/mean(disease ==1)

library(plotly)
data("heights")
plot <- heights %>%
    mutate(height = round(height)) %>%
    group_by(height) %>%
    summarize(p=mean(sex=="Male")) %>%
    qplot(height, p, data =.) + geom_smooth()

ggplotly(plot)

ps <- seq(0, 1, 0.1)
plot <- heights %>%
    mutate(g = cut(height, quantile(height, ps), include.lowest=TRUE )) %>%
    group_by(g) %>%
    summarize(p=mean(sex=="Male"), height=mean(height)) %>%
    qplot(height, p, data =.) + geom_smooth()

ggplotly(plot)

library(MASS)
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- mvrnorm(n=10000, c(69,69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))

plot <- dat %>%
    qplot(x,y,data =.)
ggplotly(plot)

ps <- seq(0,1,0.1)
plot <- dat %>%
    mutate(g = cut(x, quantile(x,ps), include.lowest = TRUE)) %>%
    group_by(g) %>%
    summarize(y = mean(y), x =mean(x)) %>%
    qplot(x,y,data =.)
ggplotly(plot)

# 27.8 
data("mnist_27")

plot <- mnist_27$train %>%
    ggplot(aes(x_1, x_2, color = y)) + geom_point()

ggplotly(plot)

# linear regression
fit <- mnist_27$train %>%
    mutate(y = ifelse(y==7, 1, 0)) %>%
    lm(y ~ x_1 + x_2, data = .)

p_hat <- predict(fit, newdata = mnist_27$test, type = "response") #probability
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

cm <- confusionMatrix(y_hat,mnist_27$test$y)

#plot the true values
plot <- mnist_27$true_p %>%
    ggplot(aes(x_1, x_2, z=p, fill = p)) +
    geom_raster() +
    scale_fill_gradientn(colors = c("#F8766D", "white", "00BFC4")) +
    stat_contour(breaks=c(0.5), color ="black")
ggplotly(plot)    


#linear regression comprehension check
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0,0.5,0.5,1.0),2,2)
dat <- MASS::mvrnorm(n, c(69,69), Sigma) %>%
        data.frame() %>%
        setNames(c("x", "y"))


my_partition <- function() {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]

    fit <- train %>%
        lm(y ~ x, data = .)
    y_hat <- predict(fit, newdata = test, type = "response")
    
    rmse(test$y, y_hat)
}
set.seed(1)
list_of_rmses <- replicate(100, my_partition())
mean(list_of_rmses)
sd(list_of_rmses)

# Generic
runsim <- function(n) {
    Sigma <- 9*matrix(c(1.0,0.5,0.5,1.0),2,2)
    dat <- MASS::mvrnorm(n, c(69,69), Sigma) %>%
        data.frame() %>%
        setNames(c("x", "y"))
    rmses <- replicate(100, {
        test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
        test <- dat[test_index,]
        train <- dat[-test_index,]

        fit <- train %>%
            lm(y ~ x, data = .)
        y_hat <- predict(fit, newdata = test, type = "response")
        
        rmse(test$y, y_hat)
    })
    c(mean(rmses), sd(rmses))
}
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
sapply(n, runsim)

# With higher correlation
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0,0.95,0.95,1.0),2,2)
dat <- MASS::mvrnorm(n, c(69,69), Sigma) %>%
        data.frame() %>%
        setNames(c("x", "y"))


my_partition <- function() {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]

    fit <- train %>%
        lm(y ~ x, data = .)
    y_hat <- predict(fit, newdata = test, type = "response")
    
    rmse(test$y, y_hat)
}
set.seed(1)
list_of_rmses <- replicate(100, my_partition())
mean(list_of_rmses)
sd(list_of_rmses)

#with two features
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0,0.75,0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0),3,3)
dat <- MASS::mvrnorm(n, c(0,0,0), Sigma) %>%
        data.frame() %>%
        setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]

test <- dat %>% slice(test_index)
train <- dat %>% slice(-test_index)

fit_x1 <- train %>%
    lm(y ~ x_1, data = .)
y_hat_x1 <- predict(fit_x1, newdata = test, type = "response")

fit_x2 <- train %>%
    lm(y ~ x_2, data = .)
y_hat_x2 <- predict(fit_x2, newdata = test, type = "response")

fit_both <- train %>%
    lm(y ~ x_1 + x_2, data = .)
y_hat_both <- predict(fit_both, newdata = test, type = "response")

rmse(test$y, y_hat_both)
rmse(test$y, y_hat_x1)
rmse(test$y, y_hat_x2)

sqrt(mean((y_hat_both - test$y)^2))
