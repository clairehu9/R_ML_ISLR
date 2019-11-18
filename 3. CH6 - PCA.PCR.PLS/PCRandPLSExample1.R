rm(list=ls())
options(warn=-1)   # Supress warning messages
###############################################
### Functions
###############################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
# the pls package contains the pls() and plse() functions
needed <- c("pls", "ISLR")  
installIfAbsentAndLoad(needed)
# Need to do this first:
Hitters <- na.omit(Hitters)
x <- model.matrix(Salary~., Hitters)[, -1]
y <- Hitters$Salary
# Lab starts here
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
set.seed(2)
##################################################
# Principal Component Regression
##################################################
pcr.fit <- pcr(Salary~., 
             data=Hitters, 
             scale=TRUE, 
             validation="CV")
# Setting validation="CV" causes pcr() to compute the 
# ten-fold cross-validation error for each possible value of
# M
summary(pcr.fit)
# The summary() function also provides the percentage of
# variance explained in the predictors and in the response
# using different numbers of components. We can think of
# this as the amount of information about the predictors or 
# the response that is captured using M principal
# components. Note that pcr() reports the root mean squared
# error; in order to obtain the usual MSE, we must square
# this quantity.
validationplot(pcr.fit, val.type="MSEP")
# The smallest cross-validation error occurs when M = 16
# components are used. However, from the plot we also see
# that the cross-validation error is roughly the same when
# only one component is included in the model. This suggests
# that a model that uses just a small number of components
# might suffice.

# Perform PCR on the training data and evaluate its test set
# performance.
set.seed(1)
pcr.fit <- pcr(Salary~., 
             data=Hitters, 
             subset=train, 
             scale=TRUE, 
             validation="CV")
validationplot(pcr.fit, val.type="MSEP")
# The lowest cross-validation error occurs when M = 7 
# component are used. We compute the test MSE as follows.
pcr.pred <- predict(pcr.fit, x[test, ], ncomp=7)
mean((pcr.pred-y.test)^2)
# This test set MSE is competitive with the results obtained
# using ridge regression and the lasso. However, as a result
# of the way PCR is implemented, the final model is more
# difficult to interpret because it does not perform any
# kind of variable selection  or even directly produce
# coefficient estimates.
# 
# Finally, we fit PCR on the full data set, using M = 7, the
# number of components identified by cross-validation.
pcr.fit <- pcr(y ~ x, scale=TRUE, ncomp=7)
summary(pcr.fit)
# 
# 
# The pcr() function contains a named matrix called 
# coefficients that provides the betas - that is, the 
# coefficients of the lm() model that would produce the same
# predictions that were produced for each value of M. This 
# is illustrated in the code that follows:
# 
# Create a pcr() model with all 19 values of M. Here, 
# scaling is turned off - an example with scaled data
# follows hereafter.
my.Hitters <- na.omit(Hitters)
my.pcr.fit <- pcr(Salary~., 
                  data=my.Hitters, 
                  scale=F, 
                  validation="CV")
# Predict using a model based on only the first principal
# component (M = 1)
my.pcr.predict <- predict(my.pcr.fit, ncomp=1)
# Set up a df for a linear regression which regresses the
# prediction onto the original (unscaled) x's
newdf <- data.frame(my.Hitters[-19], 
                    "NewSalary"=my.pcr.predict[, 1, 1])
# Regress the prediction onto the original (unscaled) x's
my.lm.fit <- lm(NewSalary~., data=newdf)
# Compare these lm() coefficients with those stored in the
# my.pcr.fit object
data.frame("lm"=coef(my.lm.fit)[-1],
           "PCR"=my.pcr.fit$coefficients[, 1, 1])

# We can show the same for scaled x's as follows (here we 
# compare the coefficients for a model using the first 5 
# principal components - that is, M = 5):
# 
# Perform the pcr() for all 19 possible values of M (same as
# above but with scaling on)
my.pcr.fit <- pcr(Salary~., 
                  data=my.Hitters, 
                  scale=T, 
                  validation="CV")
# Predict using a model based on only the first principal
# component (M = 5)
my.pcr.predict <- predict(my.pcr.fit, ncomp=5)
# Set up a new df with scaled x's but unscaled y
my.x <- scale(model.matrix(Salary~., my.Hitters)[, -1])
my.y <- my.Hitters$Salary
# Regress the prediction onto the original (scaled) x's
lm.fit <- lm(my.pcr.predict[, 1, 1]~my.x)
# Compare these lm() coefficients with those stored in the
# my.pcr.fit object for M = 5
data.frame("lm"=coef(lm.fit)[-1],
           "PCR"=my.pcr.fit$coefficients[, 1, 5])

##################################################
# Partial Least Square Regression
##################################################
# Partial Least Squares
set.seed(1)
pls.fit <- plsr(Salary ~ ., 
              data=Hitters, 
              subset=train, 
              scale=TRUE, 
              validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
# The lowest cross-validation error occurs when only M = 2
# partial least squares directions are used. We now evaluate
# the corresponding test set MSE.
pls.pred <- predict(pls.fit, x[test, ], ncomp=2)
mean((pls.pred-y.test)^2)
# The test MSE is comparable to, but slightly higher than, 
# the test MSE obtained using ridge regression, the lasso, 
# and PCR.
# 
# Finally, we perform PLS using the full data set, using M =
# 2, the number of components identified by 
# cross-validation.
pls.fit <- plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)
# Notice that the percentage of variance in Salary that the 
# two-component PLS fit explains is almost as much as that 
# explained using the final seven-component model PCR fit, 
# This is because PCR only attempts to maximize the amount 
# of variance explained in the predictors, while PLS 
# searches for directions that explain variance in both the 
# predictors and the response.
options(warn=0)   # Restore warning messages
