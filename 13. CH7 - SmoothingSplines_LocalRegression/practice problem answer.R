#######################################
#### Smoothing Splines  ###############
#######################################


#######################################
#### TRUE/ FALSE ANSWER ###############
#######################################
# 1. True
# 2. False
# 3. False
# better use generalized instead of LOOCV when having duplicate points.
# 


rm(list=ls())

#######################
### Functions
#######################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    library(thispackage, character.only = T)
  }
}
##############################
### Questions#################
##############################
## MASS contains the Boston data
needed  <-  c('ISLR', 'splines','MASS')      
installIfAbsentAndLoad(needed)
attach(Boston)
# The smooth.spline() function doesn't like duplicate
# x-values when cross-validating, so we first remove the
# rows from lstat and crim that are associated with
# duplicate lstat (the x's) values.
duplicate.x.indices <- which(duplicated(lstat))
lstat <- lstat[-duplicate.x.indices]
crim <- crim[-duplicate.x.indices]
lstatlims <- range(lstat)
lstat.grid <- seq(from=lstatlims[1], to=lstatlims[2])
fit <- smooth.spline(lstat, crim, df=30)
fit2 <- smooth.spline(lstat, crim, cv=TRUE)
fit2$df

plot(lstat, crim, xlim=lstatlims, cex=.5, col="darkgrey")
title("Cubic Spline VS Smoothing Spline")
lines(fit, col="blue", lwd=2)
lines(fit2, col="red", lwd=2)
legend("topright",c("30 df","cv df of 12"),col=c("red","darkgreen"),lwd=2)
abline(v=c(10,20,30),lty=2,col="darkgreen")

#######################################
#### Local Regression #################
#######################################

#######################################
#### TRUE/ FALSE ANSWER ###############
#######################################
# 1) False - increasing the span DECREASES the variance in the model
# 2) True
# 3) True - If two points are tied for the furthest distance, they are both weighted as 0
# 4) False - It is a memory based model
# 5) False - Not anymore

#######################################
#### Questions ########################
#######################################
# First Question
fit1 <- loess(crim~lstat, span = 0.25)
mse1 <- mean((fit1$fitted - crim)^2)

fit2 <- loess(crim~lstat, span = 0.5)
mse2 <- mean((fit2$fitted - crim)^2)

plotting_grid <- seq(min(lstat),max(lstat), by = 0.5)
plot(lstat, crim, xlim=lstatlims, cex=.5, col="darkgrey")
title("Local Regression")
# Plot fitted values with x as a grid of values
lines(plotting_grid, predict(fit1, data.frame(lstat=plotting_grid)), col="red", lwd=2)
lines(plotting_grid, predict(fit2, data.frame(lstat=plotting_grid)), col="blue", lwd=2)
legend("topright", legend=c("Span=0.2", "Span=0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)


# Second Question
n=dim(Boston)[1]
train.indices = sample(1:n,(n*.8))
test.indices = (1:n)[-train.indices]
x <- lstat
y <- crim

# Set up the testing and training data sets
train.x <- x[train.indices]
train.y <- y[train.indices]
test.x <- x[test.indices]
test.y <- y[test.indices]
trainset <- data.frame(cbind(train.x,train.y))
testset <- data.frame(cbind(test.x, test.y))

n <- nrow(trainset)
mydf <- trainset[sample(1:n, n),]

# n folds is LOOCV
# We'll use something smaller
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

# Values for the span
numspan <- seq(0.1,1,0.05)

mse <- rep(0, length(numspan))
for(i in 1:length(numspan)){
    mse2 <- rep(0, numfolds)
    for(z in 1:numfolds){
        test_ind <- which(fold.indices == z) # selects the testing indices that are not included
        train_ind<- -test_ind # things that aren't test are the training
        train.data <- mydf[train_ind, ] # set training data
        test.data <- mydf[train_ind, ] # set testing data
        # Fit the model using the numspan (that represents the number of values used)
        loessfit <- loess(train.data[,2] ~ train.data[,1], span=numspan[i], degree=2, family="symmetric")
        pred.text <- predict(loessfit, test.data[,1])
        # It's necessary to include na.rm=T in the following
        # calculation because predict cannot predict an x
        # that is outside the range of the x's used to
        # construct the model. This can happen frequently
        # due to the random subsetting of the x's
        mse2[z] <- mean((pred.text - test.data[,2])^2, na.rm=T)
    }
    mse[i] <- min(mse2)
}
best.span.10 <- numspan[which.min(mse)]
best.mse.10 <- min(mse)


numfolds <-25
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

# Values for the span
numspan <- seq(0.1,1,0.05)

mse <- rep(0, length(numspan))
for(i in 1:length(numspan)){
    mse2 <- rep(0, numfolds)
    for(z in 1:numfolds){
        test_ind <- which(fold.indices == z) # selects the testing indices that are not included
        train_ind<- -test_ind # things that aren't test are the training
        train.data <- mydf[train_ind, ] # set training data
        test.data <- mydf[train_ind, ] # set testing data
        # Fit the model using the numspan (that represents the number of values used)
        loessfit <- loess(train.data[,2] ~ train.data[,1], span=numspan[i], degree=2, family="symmetric")
        pred.text <- predict(loessfit, test.data[,1])
        # It's necessary to include na.rm=T in the following
        # calculation because predict cannot predict an x
        # that is outside the range of the x's used to
        # construct the model. This can happen frequently
        # due to the random subsetting of the x's
        mse2[z] <- mean((pred.text - test.data[,2])^2, na.rm=T)
    }
    mse[i] <- min(mse2)
}
best.span.25 <- numspan[which.min(mse)]
best.mse.25 <- min(mse)

numfolds <- 50
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

# Values for the span
numspan <- seq(0.1,1,0.05)

mse <- rep(0, length(numspan))
for(i in 1:length(numspan)){
    mse2 <- rep(0, numfolds)
    for(z in 1:numfolds){
        test_ind <- which(fold.indices == z) # selects the testing indices that are not included
        train_ind<- -test_ind # things that aren't test are the training
        train.data <- mydf[train_ind, ] # set training data
        test.data <- mydf[test_ind, ] # set testing data
        # Fit the model using the numspan (that represents the number of values used)
        loessfit <- loess(train.data[,2] ~ train.data[,1], span=numspan[i], degree=2, family="symmetric")
        pred.text <- predict(loessfit, test.data[,1])
        # It's necessary to include na.rm=T in the following
        # calculation because predict cannot predict an x
        # that is outside the range of the x's used to
        # construct the model. This can happen frequently
        # due to the random subsetting of the x's
        mse2[z] <- mean((pred.text - test.data[,2])^2, na.rm=T)
    }
    mse[i] <- min(mse2)
}
best.span.50 <- numspan[which.min(mse)]
best.mse.50 <- min(mse)


best.span.10
best.span.25
best.span.50

best.mse.10
best.mse.25
best.mse.50

