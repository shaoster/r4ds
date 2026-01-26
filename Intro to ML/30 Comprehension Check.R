library(dslabs)
library(caret)
library(tidyverse)
library(plotly)
library(gam)

set.seed(1996)
n <- 10^3 #number of records
p <- 10^4 #number of features
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[,sample(p,100)]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
    pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal = TRUE)$p.value
}

ind <- which(pvals <=0.01)
length(ind)

x_sig_subset <- x[,ind]
set.seed(1)
fit_sig <- train(x_sig_subset, y, method = "glm")
fit_sig$results

