library(dslabs)
library(dplyr)
library(lubridate)
library(purrr)
library(caret)
library(tidyverse)
library(plotly)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
	s <- str_trim(s)
	header_index <- str_which(s, "2015")[1]
	tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
	month <- tmp[1]
	header <- tmp[-1]
	tail_index  <- str_which(s, "Total")
	n <- str_count(s, "\\d+")
	out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
	s[-out] %>%
		str_remove_all("[^\\d\\s]") %>%
		str_trim() %>%
		str_split_fixed("\\s+", n = 6) %>%
		.[,1:5] %>%
		as_tibble(.name_repair = "unique") %>% 
		setNames(c("day", header)) %>%
		mutate(month = month,
			day = as.numeric(day)) %>%
		gather(year, deaths, -c(day, month)) %>%
		mutate(deaths = as.numeric(deaths))
}) %>%
	mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                          "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
	mutate(date = make_date(year, month, day)) %>%
    dplyr::filter(date <= "2018-05-01") %>%
    mutate(date_numeric = as.numeric(date))

total_days <- diff(range(dat$date_numeric))
span <- 60/total_days
clean_dat <- na.omit(dat)
fit_1 <- loess(deaths ~ date_numeric, degree = 1, span=span, data = clean_dat)
#fit_2 <- loess(margin ~ day, span=span, data=polls_2008)

plot <- clean_dat %>%
    mutate(smooth_1 = fit_1$fitted) %>%
    ggplot(aes(date_numeric, deaths)) +
    geom_point(size=3, alpha=0.5, color ="grey") +
    geom_line(aes(date_numeric, smooth_1), color="red", lty =1)
ggplotly(plot)

fit_2 <- loess(deaths ~ date_numeric, degree = 1, span=span, data = clean_dat)
plot <- clean_dat %>%
    mutate(smooth_1 = fit_2$fitted) %>%
    ggplot(aes(yday(date), deaths)) +
    geom_point(size=3, alpha=0.5, color ="grey") +
    geom_line(aes(col=year, x=yday(date), y=smooth_1), lty = 1)
ggplotly(plot)

library(broom)
train <- mnist_27$train %>% mutate(y = ifelse(y==7, 1, 0))
fit_3 <- loess(y ~ x_2, data = train, degree = 1)
p_hat <- predict(fit_3, newdata = mnist_27$test, type = "response") #probability
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
cm <- confusionMatrix(y_hat,mnist_27$test$y)
