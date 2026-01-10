library(dslabs)
library(dplyr)
library(lubridate)
library(purrr)
library(caret)
library(tidyverse)
library(plotly)

data("polls_2008")
plot <- qplot(day,margin, data=polls_2008)
ggplotly(plot)

fit <- lm(margin~day, data = polls_2008)
resid <- ifelse(fit$resid >0, "+","-")

plot <- polls_2008 %>%
        mutate(resid = resid) %>%
        ggplot(aes(day, margin)) +
        geom_smooth(method = "lm", se=TRUE, color="black") +
        geom_point(aes(color = resid), size =1)
ggplotly(plot)

#bin smoothers
span <- 3.5
tmp <- polls_2008 %>%
    crossing(center = polls_2008$day) %>%
    mutate(dist = abs(day-center)) %>%
    filter(dist<=span)

plot <- tmp %>% filter(center %in% c(-125,25)) %>%
    ggplot(aes(day, margin)) +
    geom_point(data=polls_2008, size=3, alpha =0.5, color ="grey") +
    geom_point(size=2) +
    geom_smooth(aes(group=center), method ="lm",formula = y~1, se=FALSE) +
    facet_wrap(~center)
ggplotly(plot)

#larger span
span <- 7
fit <- with(polls_2008,
        ksmooth(day, margin, kernel = "box", bandwidth = span))

plot <- polls_2008 %>%
        mutate(smooth = fit$y) %>%
        ggplot(aes(day, margin)) +
            geom_point(size = 3, alpha=0.5, color = "grey") +
        geom_line(aes(day, smooth), color ="red")
ggplotly(plot)

#kernel
span <- 7
fit <- with(polls_2008,
        ksmooth(day, margin, kernel = "normal", bandwidth = span))

plot <- polls_2008 %>%
        mutate(smooth = fit$y) %>%
        ggplot(aes(day, margin)) +
            geom_point(size = 3, alpha=0.5, color = "grey") +
        geom_line(aes(day, smooth), color ="red")
ggplotly(plot)

plot <- polls_2008 %>% 
    ggplot(aes(day, margin)) +
    geom_point() +
    geom_smooth(color = "red", span =0.2, method = "loess", method.arg = list(degree=1), se=FALSE)
ggplotly(plot)

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree = 1, span=span, data = polls_2008)
fit_2 <- loess(margin ~ day, span=span, data=polls_2008)

plot <- polls_2008 %>%
    mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
    ggplot(aes(day, margin)) +
    geom_point(size=3, alpha=0.5, color ="grey") +
    geom_line(aes(day, smooth_1), color="red", lty =2) + #line style
    geom_line(aes(day, smooth_2), color="orange", lty =1)
ggplotly(plot)

plot <- polls_2008 %>%
    ggplot(aes(day, margin)) +
    geom_point()+
    geom_smooth()
ggplotly(plot)