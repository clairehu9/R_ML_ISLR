rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
##############################
### Load required packages ###
##############################
# 
needed <- c('ISLR', "MASS")      
installIfAbsentAndLoad(needed)
attach(Wage)
agelims = range(age)
age.grid = seq(from=agelims[1], to=agelims[2])
plot(age,wage,xlim=agelims, cex=.5, col="darkgrey")
title("Local Regression")
fit = loess(wage ~ age, span =.2, data=Wage)
fit2 = loess(wage ~ age, span =.5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), 
      col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)),
      col="blue", lwd=2)
legend("topright", legend=c("Span = 0.2", "Span=0.5"),
       col=c("red", "blue"), lty=1, lwd=2, cex=.8)

#########################################################################

head(Boston)
plot(Boston$lstat, Boston$medv)

set.seed(5082)
n=dim(Boston)[1]
train.indices = sample(1:n,(n*.8))
test.indices = (1:n)[-train.indices]
y <- Boston$medv
x <- Boston$crim

train.x <- x[train.indices]
train.y <- y[train.indices]
test.x <- x[test.indices]
test.y <- y[test.indices]
trainset <- data.frame(cbind(train.x,train.y))
testset <- data.frame(cbind(test.x, test.y))

n <- nrow(trainset)
mydf <- trainset[sample(1:n, n),]

numfolds <- n
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

numspan <- seq(0.1,1,0.05)

mse <- rep(0, length(numspan))
for(i in 1:length(numspan)){
  mse2 <- rep(0, numfolds)
  for(z in 1:numfolds){
    test_ind <- which(fold.indices == z)
    train_ind<- -test_ind
    train.data <- mydf[train_ind, ]
    loessfit <- loess(train.data[,2] ~ train.data[,1], span=numspan[i], degree=2, family="symmetric", iterations=4)
    mse2[z] <- mean((loessfit$fitted - train.data[,2])^2)
  }
  mse[i] <- min(mse2)
}
best.span <- numspan[which.min(mse)]
best.mse <- min(mse)
best.span
best.mse

plot(NULL,NULL,type='n', xlim = c(0,1), ylim=c(min(mse),max(mse)))
lines(numspan, mse, type='b', col=2, pch=16)

locreg <- loess(trainset[,2] ~ trainset[,1], span=best.span, degree=2, family="symmetric", iterations=4)
mean((locreg$fitted - trainset[,2])^2)

locreg <- loess(testset[,2] ~ testset[,1], span=best.span, degree=2, family="symmetric", iterations=4, surface="direct")
mean((locreg$fitted - testset[,2])^2)

