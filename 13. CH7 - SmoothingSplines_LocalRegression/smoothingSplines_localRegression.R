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

needed <- c('ISLR', 'splines', 'fields')   
installIfAbsentAndLoad(needed)

##############################
### Smoothing Splines      ###
##############################

attach(Wage)

# Set the x axis on the plot
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])

########### smooth.spline function
?smooth.spline
# df - degree of freedom
# lambda/spar, ?? = r * 256^(3*spar - 1), could be NULL, df is used to determine lambda
# FALSE- 'generalized' cross-validation (GCV)
# TRUE - ordinary leave-one-out (TRUE)

########### Cubic VS Smoothing Spline with 16df
# fit cubic spline with x=age,y=wage
fit <- lm(wage ~ bs(age, degree=3, knots=c(25,40,60)), data=Wage)

# fit smoothing spline with x=age,y=wage
fit2 <- 
  
# plot two models
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Cubic Spline VS Smoothing Spline")
lines(age.grid,predict(fit,newdata = list(age=age.grid)),col="red",lwd=2,type="l")
lines(fit2, col="blue", lwd=2)
legend("topright",c("Smoothing Spline with 16 df","Cubic Spline"),col=c("blue","red"),lwd=2)
abline(v=c(25,40,60),lty=2,col="darkgreen")

########### Smoothing Spline with different degrees of freedom
# fit smoothing spline with 30 degrees of freedom
fit <- 
  
# fit smoothing spline with leave-one-out cross-validation
fit2 <- 
  
# find cv degrees of freedom

# plot two models
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Smoothing Spline)"
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("30 DF", "6.8 DF"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)
abline(v=c(25,40,60),lty=2,col="darkgreen")

# make a prediction


########### thin plate spline
# fit thin plate spline with x=age,y=wage
fit3 <- 

# make a prediction

    
################################
####### Local Regression #######
################################

# Building the Steps
# First create our dummy data
set.seed(42)
x <- runif(0,10, n = 50)
y <- runif(0,10, n = 50)
x0 <- 5
y0 <- 5


plot(x,y)
points(x0,y0, col = 'red', pch = 19, cex = 2)
##### Step 1 #####
q <- 6
span <- q/length(x)

##### Step 2 #####
myPoints <- rep(20, span*length(x))
myDistances <- rep(20, span*length(x))
for (i in 1:length(x)){
    temp_dist <- sqrt((x0-x[i])^2 + (y0-y[i])^2)
    
    if (temp_dist < max(myDistances)){
        mypoints[which.max(myDistances)] <- i
        mydistances[which.max(myDistances)] <- temp_dist
    }
}

points(x[myPoints], y[myPoints],col = 'green', pch = 16)

# Weighting the points
weights <- rep(0,length(x))
for (i in 1:length(myPoints)){
    weights[myPoints[i]] <- (1-(myDistances[i]/max(myDistances))^3)^3
}
weights


# We need these for prediction
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])

# loess function with the function, span and data
fit <- 
fit2 <- 


plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Local Regression")
# Plot fitted values with x as a grid of values
lines(age.grid, predict(fit, data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright", legend=c("Span=0.2", "Span=0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)


###### Using Cross Validation
set.seed(5082)
n=dim(Wage)[1]
train.indices = sample(1:n,(n*.8))
test.indices = (1:n)[-train.indices]
x <- Wage$age
y <- Wage$wage

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
        # Fit the model using the numspan (that represents the number of values used)
        loessfit <- loess(train.data[,2] ~ train.data[,1], span=numspan[i], degree=2, family="symmetric", iterations=4)
        mse2[z] <- mean((loessfit$fitted - train.data[,2])^2)
        # store the error
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


