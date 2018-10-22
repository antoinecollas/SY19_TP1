require(MASS)
library(FNN)
library(leaps)
library(e1071)

regresseur <- function(dataset){
  load("env.Rdata")
  x <- model.matrix(~.-1, data = dataset)
  y <- predict(reg, newx=x)
  return(y)
}

classifieur <- function(dataset){
  load("env.Rdata")
  z <- predict(class.mod, dataset)
  return(z)
}
