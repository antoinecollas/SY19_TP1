require(MASS)
library(e1071)

#fonction réalisant la validation croisée (avec K=10 par défaut) 
CV_eval <- function(model, data, hyperparameters=c(), fold=10){
  K <- fold
  n <- nrow(data)
  folds <- sample(1:K,n,replace=TRUE)
  mean <- 0
  errors <- rep(0, K)
  for(k in (1:K)){
    if ((model=='adl') || (model=='adq')){
      if (model=='adl'){
        model.fit <- lda(formula=as.factor(y)~., data=data[folds!=k,])
      }else{
        model.fit <- qda(formula=as.factor(y)~., data=data[folds!=k,])
      }
      pred <- predict(model.fit, data[folds==k,])
      errors[k] <- sum(data$y[folds==k]!=pred$class) / length(pred$class)
      mean <- mean + (errors[k]*length(pred$class))
    }else if ((model=='bayesien_naif') || (model=='reg_logistique')){
      if (model=='bayesien_naif'){
        model.fit <- naiveBayes(formula=as.factor(y)~., data=data[folds!=k,])
        pred <- predict(model.fit, data[folds==k,])
      }else{
        glm.fit <- glm(as.factor(y)~., data=data[folds!=k,], family=binomial)
        pred <- predict(glm.fit, newdata=data[folds==k,], type='response')>0.5
      }
      errors[k] <- sum(data$y[folds==k]!=pred) / length(pred)
      mean <- mean + (errors[k]*length(pred))
    }else if (model=='knn'){
      neigh <- hyperparameters$K
      train <- data[folds!=k,]
      train$y <- NULL 
      cl <- as.factor(data[folds!=k,]$y)
      test <- data[folds==k,]
      test$y <- NULL
      model.pred <- knn(train=data[folds!=k,], test=data[folds==k,], cl=cl, k=neigh)
      errors[k] <- sum(data$y[folds==k]!=model.pred) / length(model.pred)
      mean <- mean + (errors[k]*length(model.pred))
    }else if((model=='reg_lineaire') || (model=='ridge_lasso')){
      if (model=='reg_lineaire') {
        reg <- lm(formula = y~., data=data[folds!=k,])
        pred <- predict(reg, newdata=data[folds==k,])
      }else{
        alpha = hyperparameters$alpha
        lambda = hyperparameters$lambda
        xapp <- model.matrix(y~., data[folds!=k,])[,2:51]
        yapp <- data[folds!=k,]$y
        reg <- glmnet(xapp,yapp,lambda=lambda, alpha=alpha, standardize=TRUE)
        xtst <- model.matrix(y~., data[folds==k,])[,2:51]
        pred <- predict(reg,newx=xtst)
      }
      errors[k] <- sum((data$y[folds==k]-pred)^2) / length(pred)
      mean <- mean + (errors[k]*length(pred))
    }else{
      stop('Le modèle demandé n\'est pas implémenté!')
    }
  }
  mean <- mean/n
  sd <- sqrt((1/(K-1))*sum((errors - rep(mean, K))^2))
  return(c(mean, sd))
}

