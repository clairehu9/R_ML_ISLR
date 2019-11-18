rm(list=ls())
#########################################################
### Functions
#########################################################
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
needed <- c('e1071')      
installIfAbsentAndLoad(needed)
set.seed(1)
# The e1071 package contains the svm() function, which can
# be used to fit a support vector classifier when the
# argument kernel="linear" is used.
# 
# This function uses a slightly different formulation from
# (9.14) and (9.25) for the support vector classifier -
# instead of a budget, a cost is used. In the text, a high 
# budget meant a wider margin, whereas here, a high cost
# means a smaller margin (just the oppostite of the text's
# "C".

# A cost argument allows us to specify the cost of a
# violation to the margin. When the cost argument is small,
# then the margins will be wide and many support vectors
# will be on the margin or will violate the margin. When the
# cost argument is large, then the margins will be narrow
# and there will be few support vectors on the margin or
# violating the margin.
# 
# We now use the svm() function to fit the support vector
# classifier for a given value of the cost parameter. Here
# we demonstrate the use of this function on a
# two-dimensional example so that we can plot the resulting 
# decision boundary.
# 
# We begin by generating the observations, which belong to
# two classes.
x <- matrix(rnorm(20 * 2), ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1,] <- x[y == 1,] + 1
# We begin by checking whether the classes are linearly separable.
plot(x[, 2], x[, 1], col=(3 - y))
# They are not!
# 
# Next, we fit the support vector classifier.
# 
# Note that in order for the svm() function to perform
# classification (as opposed to SVM-based regression), we
# must encode the response as a factor.
# 
# The argument scale=FALSE tells the svm() function not to
# scale each feature to have mean zero or standard deviation
# one; depending on the application, one might prefer to use
# scale=TRUE.
dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y ~ ., data=dat, kernel="linear", cost=10, scale=FALSE)
# We can now plot the support vector classifier obtained:
plot(svmfit, dat)
# Note that the two arguments to the plot.svm() function are
# the output of the call to svm(), as well as the data used
# in the call to svm().
# 
# The region of feature space that will be assigned to the
# −1 class is shown in light blue, and the region that will
# be assigned to the +1 class is shown in purple. The
# decision boundary between the two classes is linear
# (because we used the argument kernel="linear"), though due
# to the way in which the plotting function is implemented
# in this library the decision boundary looks somewhat
# jagged in the plot.
# 
# We see that in this case only one observation  is
# misclassified. Note that here the second feature is
# plotted on the x-axis and the first feature is plotted on
# the y-axis, in contrast to the behavior of the usual
# plot() function in R.) The support vectors are plotted as
# crosses and the remaining observations are plotted as
# circles; We see here that there are seven support vectors.
# We can determine their identities as follows:
svmfit$index
# We can obtain some basic information about the support
# vector classifier fit using the summary() command:
summary(svmfit)
# This tells us, for instance, that a linear kernel was used
# with cost = 10, and that there were seven support vectors,
# four in one class and three in the other. The gamma
# parameter will be discussed later when we adress
# non-linear kernels.
# 
# What if we instead used a smaller value of the cost
# parameter?
svmfit <- svm(y ~ ., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)
svmfit$index
# Now that a smaller value of the cost parameter is being
# used, we obtain a larger number of support vectors,
# because the margin is now wider. Unfortunately, the svm()
# function does not explicitly output the coefficients of 
# the linear decision boundary obtained when the support
# vector classifier is fit, nor does it output the width of
# the margin.
# 
# The e1071 library includes a built-in function, tune(), to
# perform cross validation.
# 
# By default, tune() performs ten-fold cross-validation on a
# set of models of interest. In order to use this function,
# we pass in relevant information about the set of models
# that are under consideration. The following command
# indicates that we want to compare SVMs with a linear 
# kernel, using a range of values of the cost parameter.
set.seed(1)
tune.out <- tune(svm,y ~ ., data=dat, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5, 10, 100)))
# We can easily access the cross-validation errors for each
# of these models using the summary() command:
summary(tune.out)
# We see that cost=0.1 results in the lowest 
# cross-validation error rate (the error column). The
# dispersion column is the standard deviation of the error
# rate across the folds.
# 
# The tune() function stores the best model obtained, which
# can be accessed as follows:
bestmod <- tune.out$best.model
summary(bestmod)
# The predict() function can be used to predict the class
# label on a set of test observations, at any given value of
# the cost parameter. We begin by generating a test data
# set.
xtest <- matrix(rnorm(20 * 2), ncol=2)
ytest <- sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))
# Now we predict the class labels of these test
# observations. Here we use the best model obtained through
# cross-validation in order to make predictions.
ypred <- predict(bestmod, testdat)
table(truth=testdat$y, predict=ypred)
# Thus, with this value of cost, 19 of the test observations
# are correctly classified.
# 
# What if we had instead used cost=0.01?
svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred <- predict(svmfit, testdat)
table(truth=testdat$y, predict=ypred)
# In this case one additional observation is misclassified.
#
# Now consider a situation in which the two classes are
# linearly separable. Then we can find a separating
# hyperplane using the svm() function. We first further
# separate the two classes in our simulated data so that
# they are linearly separable:
x[y==1, ] <- x[y==1, ] + 0.5
plot(x[, 2], x[, 1], col=(y + 5) / 2, pch=19)
# Now the observations are just barely linearly separable.
# We fit the support vector classifier and plot the
# resulting hyperplane, using a very large value of cost so
# that no observations are misclassified.
dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
# No training errors were made and only three support
# vectors were used. However, we can see from the figure
# that the margin is very narrow (because the observations
# that are not support vectors, indicated as circles, are
# very close to the decision boundary). It seems likely that
# this model will perform poorly on test data. We now try a
# smaller value of cost:
svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)
# Using cost=1, we misclassify a training observation, but
# we also obtain a much wider margin and make use of seven
# support vectors. It seems likely that this model will
# perform better on test data than the model with cost=1e5.
