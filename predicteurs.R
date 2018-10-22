regresseur <- function(dataset){
  load("env_regresseur.Rdata")
  x <- model.matrix(~.-1, data = dataset)
  y <- predict(reg, newx=x)
  return(y)
}