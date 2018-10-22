require(MASS)
library(FNN)
library(leaps)
library(e1071)

#fonction réalisant la validation croisée (avec K=10 par défaut)
CV_eval2 <- function(model, data, fold=10){
  K <- fold
  n <- nrow(data)
  #m <- 2*length(model)
  m <- length(model)
  folds <- sample(1:K,n,replace=TRUE)
  mean <- rep(0, m)
  sd <- rep(0, m)
  errors <- matrix(0, m, K)
  #data$y <- data$y %%2
  
  sub <- regsubsets(y~.,data=data,method='backward');
  a <- summary(sub);
  coefs <- coef(sub, which.min(a$bic))
  colnames <- attributes(coefs)
  arg <- c(unlist(colnames, use.names=FALSE),"y")
  subset <- data[,arg[2:length(arg)]]
  
  for(k in (1:K)){

    modNo <- 1

    for (mod in model){
      if ((mod=='adl') || (mod=='adq')){
        if (mod=='adl'){
          #model.fit <- lda(formula=as.factor(y)~., data=data[folds!=k,])
          subset.fit <- lda(formula=as.factor(y)~., data=subset[folds!=k,])
        }else{
          #model.fit <- qda(formula=as.factor(y)~., data=data[folds!=k,])
          subset.fit <- qda(formula=as.factor(y)~., data=subset[folds!=k,])
        }
        #model.pred <- predict(model.fit, data[folds==k,])
        #errors[modNo,k] <- sum(data$y[folds==k]!=model.pred$class) / length(model.pred$class)
        #mean[modNo] <- mean[modNo] + (errors[modNo,k]*length(model.pred$class))

        #modNo<- modNo +1
        subset.pred <- predict(subset.fit, subset[folds==k,])
        #print(subset.pred$class)
        #print(data$y[folds==k])
        errors[modNo,k] <- sum(data$y[folds==k]!=subset.pred$class) / length(subset.pred$class)
        mean[modNo] <- mean[modNo] + (errors[modNo,k]*length(subset.pred$class))
        modNo<- modNo +1

      }else if ((mod=='bayesien_naif') || (mod=='reg_logistique')){
        if (mod=='bayesien_naif'){
            #model.fit <- naiveBayes(formula=as.factor(y)~., data=data[folds!=k,])
            subset.fit <- naiveBayes(formula=as.factor(y)~., data=subset[folds!=k,])
            #model.pred <- predict(model.fit, data[folds==k,])
            subset.pred <- predict(subset.fit, subset[folds==k,])
        }else{
            #model.fit <- glm(as.factor(y)~., data=data[folds!=k,], family=binomial)
            subset.fit <- glm(as.factor(y)~., data=subset[folds!=k,], family=binomial)
            #model.pred <- predict(model.fit, newdata=data[folds==k,], type='response')>0.5
            subset.pred <- predict(subset.fit, newdata=subset[folds==k,], type='response')>0.5
        }

        #errors[modNo,k] <-sum(data$y[folds==k]!=model.pred) / length(model.pred)
        #mean[modNo] <- mean[modNo] + (errors[modNo,k]*length(model.pred))

        #modNo<- modNo +1
        errors[modNo,k] <- sum(data$y[folds==k]!=subset.pred) / length(subset.pred)
        mean[modNo] <- mean[modNo] + (errors[modNo,k]*length(subset.pred))
        modNo<- modNo +1

      }else if(mod=='reg_lineaire'){

        #model.fit <- lm(formula=y~., data=data[folds!=k,])
        subset.fit <- lm(formula=y~., data=subset[folds!=k,])

        #model.pred <- predict(model.fit, data[folds==k,])
        #errors[modNo,k] <- sum((data$y[folds==k]-model.pred)^2) / length(model.pred)
        #mean[modNo] <- mean[modNo] + (errors[modNo,k]*length(model.pred))

        #modNo<- modNo +1
        subset.pred <- predict(subset.fit, subset[folds==k,])
        errors[modNo,k] <- sum((data$y[folds==k]-subset.pred)^2) / length(subset.pred)
        mean[modNo] <- mean[modNo] + (errors[modNo,k]*length(subset.pred))
        modNo<- modNo +1

      }else if(mod=='knn'){
        errors[modNo,k] <- 1
        #errors[modNo+1,k] <- 1
        for(neigh in 1:15){
          #model.pred <- knn.reg(train=data[folds!=k,], test=data[folds==k,], y=data$y[folds!=k], neigh)
          subset.pred <- knn.reg(train=subset[folds!=k,], test=subset[folds==k,], y=subset$y[folds!=k], neigh)
          #err <- sum(data$y[folds==k]!=model.pred$pred) / length(model.pred$pred)
          err.sub <- sum(subset$y[folds==k]!=subset.pred$pred) / length(subset.pred$pred)
          #if (err < errors[modNo,k]) errors[modNo,k] <- err
          #if (err.sub < errors[modNo+1,k]) errors[modNo+1,k] <- err.sub
          if (err.sub < errors[modNo,k]) errors[modNo,k] <- err.sub
        }

        #mean[modNo] <- mean[modNo] + (errors[modNo,k]*length(model.pred$pred))
        #modNo<- modNo +1

        mean[modNo] <- mean[modNo] + (errors[modNo,k]*length(subset.pred$pred))
        modNo<- modNo +1

      }
      else{
        stop('Le modèle demandé n\'est pas implémenté!')
      }
    }
  }
  for (i in 1:m) {
    mean[i] <- mean[i]/n
  }
  for (i in 1:m) {
    sd[i] <- sqrt((1/(K-1))*sum((t(errors[i,]) - rep(mean[i], K))^2))
  }
  return(cbind(mean, sd))
}
