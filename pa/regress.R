library(pa)
library(tidyverse)
library(waterfalls)
library(factorAnalytics)

## Single-period regression analysis
data(jan)

jan %>% glimpse()
jan %>% as_tibble()
jan$date %>% unique()

r1 <-my_regress(x = jan, date.var = "date", ret.var = "return", reg.var = c("sector", "value", "growth"), 
                benchmark.weight = "benchmark",portfolio.weight = "portfolio")

summary(r1)
glimpse(r1)
r1@contrib
r1@universe
r1@portfolio.ret

my_regress <- function (x, date.var = "date", ret.var = "return", reg.var = c("sector", "value", "growth"), 
                        benchmark.weight = "benchmark", portfolio.weight = "portfolio") 
{
  stopifnot(is.data.frame(x))
  stopifnot(length(benchmark.weight) == 1)
  stopifnot(length(portfolio.weight) == 1)
  stopifnot(length(ret.var) == 1)
  stopifnot(length(date.var) == 1)
  dates <- unique(x[[date.var]])
  len <- length(dates)
  if (len > 1) {
    .fun <- function(i) {
      my_regress(x[x[[date.var]] %in% i, ], date.var, ret.var, 
                 reg.var, benchmark.weight, portfolio.weight)
    }
    multiples <- lapply(dates, .fun)
    reg.multi <- new("regressionMulti", date.var = as.character(dates), 
                     ret.var = ret.var, reg.var = reg.var, benchmark.weight = benchmark.weight, 
                     portfolio.weight = portfolio.weight, universe = multiples)
    benchmark.ret.mat <- NULL
    for (i in 1:len) {
      benchmark.ret.mat <- cbind(benchmark.ret.mat, multiples[[i]]@benchmark.ret)
    }
    colnames(benchmark.ret.mat) <- as.character(dates)
    reg.multi@benchmark.ret <- benchmark.ret.mat
    portfolio.ret.mat <- NULL
    for (i in 1:len) {
      portfolio.ret.mat <- cbind(portfolio.ret.mat, multiples[[i]]@portfolio.ret)
    }
    colnames(portfolio.ret.mat) <- as.character(dates)
    reg.multi@portfolio.ret <- portfolio.ret.mat
    act.ret.mat <- NULL
    for (i in 1:len) {
      act.ret.mat <- cbind(act.ret.mat, multiples[[i]]@act.ret)
    }
    colnames(act.ret.mat) <- as.character(dates)
    reg.multi@act.ret <- act.ret.mat
    coeff.mat <- NULL
    for (i in 1:len) {
      coeff.mat <- cbind(coeff.mat, multiples[[i]]@coefficients)
    }
    colnames(coeff.mat) <- as.character(dates)
    reg.multi@coefficients <- coeff.mat
    act.expo <- NULL
    for (i in 1:len) {
      act.expo <- cbind(act.expo, multiples[[i]]@act.expo)
    }
    colnames(act.expo) <- as.character(dates)
    reg.multi@act.expo <- act.expo
    contrib <- NULL
    for (i in 1:len) {
      contrib <- cbind(contrib, multiples[[i]]@contrib)
    }
    colnames(contrib) <- as.character(dates)
    reg.multi@contrib <- contrib
    return(reg.multi)
  }
  else {
    stopifnot(all(sapply(c(reg.var, benchmark.weight, portfolio.weight), 
                         is.character)))
    stopifnot(all(c(reg.var, benchmark.weight, portfolio.weight) %in% 
                    names(x)))
    stopifnot(is.numeric(x[[benchmark.weight]]))
    stopifnot(is.numeric(x[[portfolio.weight]]))
    benchmark.ret <- x[[benchmark.weight]] %*% x[[ret.var]]
    portfolio.ret <- x[[portfolio.weight]] %*% x[[ret.var]]
    act.ret <- portfolio.ret - benchmark.ret
    var.input <- .formula.make(ret.var, c(reg.var, "- 1"))
    lm.model <- lm(var.input, data = x)
    lm.mat <- model.matrix(var.input, data = x)
    factor.ret <- lm.model$coefficients
    act.weight <- x[[portfolio.weight]] - x[[benchmark.weight]]
    act.expo <- apply(act.weight * lm.mat, 2, sum)
    contrib <- act.expo * factor.ret
    reg <- new("regression", date.var = date.var, ret.var = ret.var, 
               reg.var = reg.var, benchmark.weight = benchmark.weight, 
               portfolio.weight = portfolio.weight, universe = x)
    reg@coefficients <- factor.ret
    reg@benchmark.ret <- benchmark.ret
    reg@portfolio.ret <- portfolio.ret
    reg@act.ret <- act.ret
    reg@act.expo <- act.expo
    reg@contrib <- contrib
    return(reg)
  }
}

.formula.make <- function(vdep, vexp) {
  formula(paste(vdep, "~", paste(vexp, collapse = " + ")))
}

library(broom)
lm.model %>% glance()
lm.model %>% tidy()
