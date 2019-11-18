rm(list=ls())
# options(warn=-1)   # Supress warning messages
####################################################
### Functions
####################################################
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
# The glmnet package contains functionality for both Ridge
# and Lasso
needed <- c('ISLR', 'glmnet')  
installIfAbsentAndLoad(needed)

###########################################################
####### Ridge Regression ##################################
###########################################################

# We will use the glmnet package in order to perform ridge 
# regression and the lasso. The main function in this 
# package is glmnet(), which can be used to fit ridge 
# regression models, lasso models, and more.
# 
# This function has slightly different syntax from other 
# model-fitting functions that we have encountered thus far.
# In particular, we must pass in an x matrix as well as a y 
# vector (or factor in the case of classification), and we 
# do not use the y ~ x syntax.
# 
# We will now perform ridge regression and the lasso in
# order to predict Salary on the Hitters data.

# The model.matrix() function is particularly useful for
# creating x; not only does it produce a matrix
# corresponding to the 19 predictors but it also 
# automatically transforms any qualitative variables into
# dummy variables. The latter property is important because
# glmnet() can only take numerical, quantitative inputs.
# 
Hitters <- na.omit(Hitters)
x <- model.matrix(Salary ~ ., Hitters)[, -1]  #We omit the intercept
y <- Hitters$Salary
############################################################
#######  Ridge Regression  #################################
############################################################
#
# The glmnet() function has an alpha argument that determines
# what type of model is fit. If alpha=0 then a ridge
# regression model is fit, and if alpha=1 (the default) then
# a lasso model is fit. We first fit a ridge regression
# model.
#
# By default the glmnet() function performs ridge regression
# for an automatically selected range of lambda values.
# However, here we have chosen to implement the function over
# a grid of 100 values ranging from lambda = 10^10 to lambda =
# 10^-2, essentially covering the full range of scenarios
# from the null model containing only the intercept, to
# values approaching the least squares fit.
#
# To accomplish this, we divide the interval from 10 to -2 
# into 100 partitions - make these the exponents of 10, so 
# the grid() vector goes from 10^10 to 0.01 in 100 
# (diminishing) steps.
#
grid <- 10 ^ seq(10, -2, length=100)
#
# As we will see, we can also compute model fits for a
# particular value of lambda that is not one of the original
# grid values.
# 
# Note that by default, the glmnet() function standardizes 
# the variables so that they are on the same scale. To turn 
# off this default setting, use the argument 
# standardize=FALSE.
# 
# Associated with each value of lambda is a vector of ridge 
# regression coefficients, stored in a matrix that can be 
# accessed by coef(). In this case, it is a 20 x 100 matrix, 
# with 20 rows (one for each of the 19 predictors, plus an 
# intercept) and 100 columns (one for each value of lambda).
                    
ridge.mod <- glmnet(x, y, 
                    alpha=0, 
                    lambda=grid)
dim(coef(ridge.mod))
plot(ridge.mod, xvar="lambda", label=T)

# We expect the coefficient estimates to be much smaller, in
# terms of l2 norm, when a large value of lambda is used, as
# compared to when a small value of lambda is used.
# 
# These are the coefficients when lambda = 10,000,000,000,
# along with their l2 norm:
ridge.mod$lambda[1]
coef(ridge.mod)[, 1]
# Compute the l2 norm of these coefficients, omitting the
# intercept
sqrt(sum(coef(ridge.mod)[-1, 1] ^ 2))
# These are the coefficients when lambda = 11,498, along with
# their l2 norm with the intercept omitted:
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
# Note the much larger l2 norm of the coefficients
# associated with the smaller value of lambda.
# 
# We can use the predict() function for a number of
# purposes. For instance, we can obtain the ridge regression
# coefficients for a new value of lambda, say 50:
predict(ridge.mod, 
        s=50, 
        type="coefficients")[1:20, ]
# 
# Note that this is different from our previous process. If,
# for example, we wanted to find the best degree for a 
# polynomial regression, our practice was to loop through a 
# vector of possible degrees, remembering the error rate for
# each. We then found the degree that produced the minimum 
# error rate and reconstructed the model with that degree 
# before using a predict function to obtain predictions.
# 
# Here, we omit the step that reconstructs the model, since 
# glmnet() remembers the coefficients for each lambda. All 
# we need to do is use predict immediately after the 
# glmnet() call, indicating to predict() which lambda we 
# wish to use (and it does not need to be one of the 
# lambda's originally provided (as the example above
# illustrates) as long as it is within the range of those
# lambdas - predict() will extrapolate).
# 
# We now split the samples into a training set and a test 
# set in order to estimate the test error of ridge 
# regression (and later, the lasso).
# 
set.seed(1)
train <- sample(1:nrow(x),  nrow(x)/2)
test <- (-train)
y.test <- y[test]
# 
# Next we fit a ridge regression model on the training set, 
# and evaluate its MSE on the test set, using lambda = 4 
# (arbitrarily). Note the use of the predict() function 
# again. With this version of the predict() function we get 
# predictions for a test set by replacing 
# type="coefficients" with the newx argument referring to
# the test x's.
# 
ridge.mod <- glmnet(x[train, ], 
                    y[train], 
                    alpha=0, 
                    lambda=grid)
ridge.pred <- predict(ridge.mod, 
                      s=4, 
                      newx=x[test, ])
# Compute the MSE
mean((ridge.pred-y.test)^2)
# 
# The test MSE is 101186. Note that if we had instead simply
# fit a model with just an intercept, we would have
# predicted each test observation using the mean of the
# training observations. In that case, we could compute the 
# test set MSE like this:
mean((mean(y[train])-y.test)^2)
# We could also get the same result by fitting a ridge
# regression model with a very large value of lambda. Note
# that 1e10 means 10 ^ 10.
ridge.pred <- predict(ridge.mod, 
                      s=1e10, 
                      newx=x[test, ])
mean((ridge.pred-y.test)^2)
# So fitting a ridge regression model with lambda = 4 leads 
# to a much lower test MSE than fitting a model with just an
# intercept.
# 
# We now check whether there is any benefit to performing 
# ridge regression with lambda = 4 instead of just 
# performing least squares regression. Recall that least 
# squares is simply ridge regression with lambda = 0
# 
# Note: In order for glmnet() to yield the exact least 
# squares coefficients when lambda = 0, we use the argument 
# exact=T when calling the predict() function. Otherwise, 
# the predict() function will interpolate over the grid of 
# lambda values used in fitting the glmnet() model, yielding
# approximate results.
# 
# When we use exact=T, there remains a slight discrepancy
# between the output of glmnet() when lambda = 0 and the
# output of lm(); this is due to numerical approximation on
# the part of glmnet().

ridge.pred <- predict(ridge.mod, 
                      s=0, 
                      newx=x[test, ], 
                      exact=T, 
                      x=x[train, ], 
                      y=y[train])
# MSE using lambda = 0 - should be close to the lm MSE
mean((ridge.pred-y.test)^2)
# The MSE of the least squares regression is 115,899, which
# is larger than the MSE for the ridge regression above with
# lambda = 4 (approx. 101,086)
# 
# Check - display the coefficients of the actual least
# squares solution
(my.lm <- lm(y ~ x,  subset=train))
# Now display the coefficients of the ridge regression with s=0
predict(ridge.mod, 
        s=0, 
        exact=T, 
        type="coefficients", 
        x=x[train, ], 
        y=y[train])[1:20, ]

# In general, if we want to fit a (unpenalized) least 
# squares model, then we should use the lm() function, since
# that function provides more useful outputs, such as 
# standard errors and p-values for the coefficients.
# 
# Also, instead of arbitrarily choosing lambda = 4, it would
# be better to use cross-validation to choose the tuning 
# parameter lambda. We can do this using the built-in 
# cross-validation function, cv.glmnet().
# 
# The function runs glmnet nfolds+1 times; the first to get 
# the lambda sequence, and then the remainder to compute the
# fit with each of the folds omitted. The error is 
# accumulated, and the average error and standard deviation 
# over the folds are computed.
# 
# By default, the cv.glmnet function performs ten-fold 
# cross-validation, though this can be changed using the 
# argument nfolds. Note that we set a random seed first for 
# repeatability.
# 
# As with the glmnet() function, if a grid of lambda's is
# not specified, the cv.glmnet() function will construct a
# sequence itself. See ?glmnet help for additional
# information on specifying lambda.

set.seed(1)
cv.out <- cv.glmnet(x[train, ], 
                    y[train], 
                    alpha=0)
plot(cv.out)
(bestlam <- cv.out$lambda.min)
# 
# Therefore, we see that the value of lambda that results in
# the smallest cross validation error is approx. 212. What
# is the test MSE associated with this value of lambda?

ridge.pred <- predict(ridge.mod, 
                      s=bestlam, 
                      newx=x[test, ])
mean((ridge.pred-y.test)^2)

# This represents a further improvement over the test MSE of
# 101,086 that we got using lambda = 4.
# 
# Finally, we refit our ridge regression model on the full
# data set, using the value of lambda chosen by
# cross-validation, and examine the coefficient estimates.

out <- glmnet(x, y, alpha=0)
ridge.coeff <- predict(out, 
                       type="coefficients", 
                       s=bestlam)[1:20, ]
data.frame("Least Squares Coefs"=coef(my.lm)[-1], "Ridge Coefs"=ridge.coeff[-1])
data.frame("Least Squares Coefs l2 Norm"=sqrt(sum(coef(my.lm)[-1]^2)), "Ridge Coefs l2 Norm"=sqrt(sum(ridge.coeff[-1]^2)))
# 
# As expected, the Ridge coefficients are smaller. Note,
# however, that none of the coefficients are zero. Ridge 
# regression does not perform variable selection!

############################################################
#######  Ridge Regression (Classification) #################
############################################################ 
# Create a factor to act as the response variable
yclass <- rep("Yes", nrow(Hitters))
yclass[Hitters$Salary < mean(Hitters$Salary)] <- "No"
yclass <- factor(yclass)
# 
# Construct a Ridge regression model using the training data
# 
ridge.mod.class <- glmnet(x[train, ], 
                          yclass[train], 
                          alpha=0, 
                          lambda=grid, 
                          family="binomial")
# 
# Compute an overall test error rate for the lambda = 4 case.
# 
ridge.pred.class <- predict(ridge.mod.class, 
                            s=4, 
                            newx=x[test, ], 
                            type="class")
mean(ridge.pred.class != yclass[test])
# 
# Compute a cross-validation error rate for each value of 
# lambda, plot them and produce a test CV error rate for the
# best lambda
# 
cv.out.class <- cv.glmnet(x[train, ], 
                          yclass[train], 
                          alpha=0, 
                          family="binomial")
plot(cv.out.class)
# 
(bestlam <- cv.out.class$lambda.min)
ridge.pred.class <- predict(ridge.mod.class, 
                            s=bestlam, 
                            newx=x[test, ], 
                            type="class")
(error.rate.ridge <- mean(ridge.pred.class != yclass[test]))
# 
# Finally, refit the ridge regression model on the full
# data set, using the value of lambda chosen by
# cross-validation, and examine the coefficient estimates.
# 
out.class.ridge <- glmnet(x, 
                          yclass, 
                          alpha=0, 
                          family="binomial")
ridge.coefficients <- predict(out.class.ridge, 
                              type="coefficients", 
                              s=bestlam)[1:20, ]
# 
############################################################
#######  The Lasso #########################################
############################################################ 

# We saw that ridge regression with a wise choice of lambda 
# can out-perform least squares as well as the null model on
# the Hitters data set. We now ask whether the lasso can 
# yield either a more accurate or a more interpretable model
# than ridge regression. In order to fit a lasso model, we 
# once again use the glmnet() function; however, this time 
# we use the argument alpha=1 (the default). Other than that
# change, we proceed just as we did in fitting a ridge
# model.

lasso.mod <- glmnet(x[train, ], 
                    y[train], 
                    alpha=1, 
                    lambda=grid)
plot(lasso.mod, xvar='lambda', label=T)

# We can see from the coefficient plot that depending on the
# choice of tuning parameter, some of the coefficients will
# be exactly equal to zero. 
# The following are the coefficients when lambda = 10,000,000,000,
# along with their l2 norm:
lasso.mod$lambda[1]
coef(lasso.mod)[, 1]
# Compute the l1 norm of these coefficients, omitting the
# intercept
sum(abs(coef(lasso.mod)[-1, 1]))
# The following are the coefficients when lambda = 100, along with
# their l1 norm with the intercept omitted:
log(67)
lasso.mod$lambda[67]
coef(lasso.mod)[, 67]
sum(abs(coef(lasso.mod)[-1, 67]))
# Observe that at lambda = 67, there are only four non-zero
# coefficients. Note also the much larger l1 norm of the
# coefficients associated with the smaller value of lambda.
# 
# We now perform cross-validation and compute the associated
# test error.
set.seed(1)
cv.out <- cv.glmnet(x[train, ], 
                    y[train], 
                    alpha=1)
plot(cv.out)
(bestlam <- cv.out$lambda.min)
lasso.pred <- predict(lasso.mod, 
                      s=bestlam, 
                      newx=x[test, ])
mean((lasso.pred-y.test)^2)

# This is substantially lower than the test set MSE of the
# null model and of least squares, and very similar to the
# test MSE of ridge regression with lambda chosen by
# cross-validation.
# 
# However, the lasso has a substantial advantage over ridge
# regression in that the resulting coefficient estimates are
# sparse. Here we see that 12 of the 19 coefficient
# estimates are exactly zero. So the lasso model with lambda 
# chosen by cross-validation contains only seven variables.
out <- glmnet(x, y, 
              alpha=1, 
              lambda=grid)
lasso.coef <- predict(out, 
                      type="coefficients", 
                      s=bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef!=0]
############################################################
#######  The following four sections apply these ##########
#######  ideas in  the classification setting     ##########
############################################################ 
yclass <- rep("Yes", length(y))
yclass[y < median(y)] <- "No"
yclass <- factor(yclass)
yclass.test <- yclass[test]
############################################################
#######  Logistic Regression (For Comparison) ##############
############################################################ 
train.df <- data.frame(x[train,], "Salary"=yclass[train])
test.df <- data.frame(x[test,], "Salary"=yclass[test])
logistic.mod.class <- glm(Salary ~ ., 
                          data=train.df, 
                          family="binomial")
logistic.pred.class <- predict(logistic.mod.class,
                               newdata = test.df, 
                               type="response")
yhat <- rep("Yes", length(logistic.pred.class))
yhat[logistic.pred.class < .5] <- "No"
(error.rate.logistic <- mean(yhat != as.character(yclass.test)))
logistic.mod.class$coefficients
############################################################
#######  Lasso (Classification) ############################
############################################################ 
lasso.mod.class <- glmnet(x[train, ], 
                          yclass[train], 
                          alpha=1, 
                          lambda=grid, 
                          family="binomial")
lasso.pred.class <- predict(lasso.mod.class, 
                            s=4, 
                            newx=x[test, ], 
                            type="class")
mean(lasso.pred.class != yclass.test)

set.seed(1)
cv.out.class <- cv.glmnet(x[train, ], 
                          yclass[train], 
                          alpha=1, 
                          family="binomial")
plot(cv.out.class)
bestlam <- cv.out.class$lambda.min
bestlam
lasso.pred.class <- predict(lasso.mod.class, 
                            s=bestlam, 
                            newx=x[test, ], 
                            type="class")
(error.rate.lasso <- mean(lasso.pred.class != yclass.test))
out.class <- glmnet(x, yclass, 
                    alpha=1, 
                    family="binomial")
lasso.coefficients <- predict(out.class, 
                              type="coefficients", 
                              s=bestlam)[1:20, ]
error.rate.logistic
error.rate.ridge
error.rate.lasso
data.frame("Logistic"=logistic.mod.class$coefficients, 
           "Ridge"=ridge.coefficients,
           "Lasso"=lasso.coefficients)
