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
 
needed <- c('e1071', 'ROCR')      
installIfAbsentAndLoad(needed)
##############################
# Support Vector Machine  ####
##############################
# In order to fit an SVM using a non-linear kernel, we once again use the svm()
# function. However, now we use a different value of the parameter kernel.
#   To fit an SVM with a polynomial kernel we use kernel="polynomial"
#   To fit an SVM with a radial kernel we use kernel="radial". 
# In the former case we also use the degree argument to specify a degree for the 
# polynomial kernel (this is d in arithmetic specification), and in the latter case  
# we use gamma to specify a value of γ for the radial basis kernel.
# 
#
# We first generate some data with a non-linear class boundary, as follows:
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol=2)
x[1:100,] <- x[1:100, ]+2
x[101:150,] <- x[101:150, ]-2
y <- c(rep(1, 150),rep(2, 50))
dat <- data.frame(x=x, y=as.factor(y))
# Plotting the data makes it clear that the class boundary
# is indeed nonlinear:
plot(x[, 2], x[, 1], col=y)
n <- length(y)
# The data is randomly split into training and testing
# groups. We now fit the training data using the svm()
# function with a radial kernel and γ = 1 and a Cost of 1.
# 
# Recall that both small Cost (pervious margins) and small
# gamma means low variance/high bias).
train <- sample(n, 100)
svmfit <- svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])
# The plot shows that the resulting SVM has a decidedly non-linear
# boundary. The summary() function can be used to obtain some
# information about the SVM fit:
summary(svmfit)
# We can see from the figure that there are a fair number of
# training errors in this SVM fit. If we increase the value
# of cost, we can reduce the number of training errors.
# However, this comes at the price of a more irregular 
# decision boundary that seems to be at risk of overfitting
# the data.
svmfit <- svm(y~., data=dat[train,], 
           kernel="radial",
           gamma=1,
           cost=1e5)
plot(svmfit,dat[train, ])
# We can perform cross-validation using tune() to select the
# best choice of γ and cost for an SVM with a radial kernel:
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial", 
              ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                          gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)
# Therefore, the best choice of parameters involves cost=1
# and gamma=2. We can view the test set predictions for this
# model by applying the predict() function to the data.
# Notice that to do this we subset the dataframe dat using
# -train as an index set.
table(true=dat[-train, "y"], 
      pred=predict(tune.out$best.model, newx=dat[-train,]))
# 39% of test observations are misclassified by this SVM.
#
#
########################################
#  ROC Curves  #########################
########################################

# The ROCR package can be used to produce ROC curves such as
# those in the text. We first write a short function to plot
# an ROC curve given a vector containing a numerical score
# for each observation, pred, and a vector containing the
# class label for each observation, truth.

rocplot <- function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
# SVMs and support vector classifiers output class labels
# for each observation. However, it is also possible to
# obtain fitted values for each observation, which are the
# numerical scores used to obtain the class labels. For
# instance, in the case of a support vector classifier, the
# fitted value for an observation X = (X1,X2, . . .,Xp)T
# takes the form β0 + β1X1 + β2X2 + . . . + βpXp. For an SVM
# with a non-linear kernel, the equation that yields the
# fitted value is given in the expression at the bottom of
# slide 12. In essence, the sign of the fitted value
# determines on which side of the decision boundary the
# observation lies. Therefore, the relationship between the
# fitted value and the class prediction for a given 
# observation is simple: if the fitted value exceeds zero
# then the observation is assigned to one class, and if it
# is less than zero than it is assigned to the other. In
# order to obtain the fitted values for a given SVM model
# fit, we use decision.values=TRUE when fitting svm(). Then
# the predict() function will output the fitted values.
svmfit.opt <- svm(y~., data=dat[train, ], 
               kernel="radial",
               gamma=2, cost=1,
               decision.values=T)
fitted <- attributes(predict(svmfit.opt,
                          dat[train, ],
                          decision.values=TRUE)
                  )$decision.values
# Now we can produce the ROC plot.
par(mfrow=c(1, 2))
rocplot(fitted,dat[train, "y"], main="Training Data")
# SVM appears to be producing accurate predictions. By increasing γ we can
# produce a more flexible fit and generate further improvements in accuracy.
svmfit.flex <- svm(y~., data=dat[train,], 
                   kernel="radial",
                   gamma=50, 
                   cost=1, 
                   decision.values=T)
fitted=attributes(predict(svmfit.flex,
                          dat[train,],
                          decision.values=T)
                  )$decision.values
rocplot(fitted,dat[train, "y"], add=T, col="red")
# However, these ROC curves are all on the training data. We are really
# more interested in the level of prediction accuracy on the test data. When
# we compute the ROC curves on the test data, the model with γ = 2 appears
# to provide the most accurate results.
fitted <- attributes(predict(svmfit.opt,
                             dat[-train,],
                             decision.values=T)
                     )$decision.values
rocplot(fitted,dat[-train, "y"], main="Test Data")
fitted <- attributes(predict(svmfit.flex,
                             dat[-train,],
                             decision.values=T)
                     )$decision.values
rocplot(fitted,dat[-train, "y"], add=T, col="red")
#
#
######################################
## SVM with Multiple Classes  ######## 
######################################
#If the response is a factor containing more than two
#levels, then the svm() function will perform multi-class
#classification using the one-versus-one approach. We
#explore that setting here by generating a third class of
#observations.
set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol=2))
y <- c(y, rep(0, 50))
x[y==0,2]=x[y == 0, 2] + 2
dat <- data.frame(x=x, y=as.factor(y))
par(mfrow=c(1, 1))
plot(x[, 2], x[, 1], col=(y + 1))
# We now fit an SVM to the data:
svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)
# The e1071 library can also be used to perform support vector regression
# if the response vector that is passed in to svm() is numerical rather than a
# factor.
#
#
########################################
#  Application to Gene Expression Data #
########################################
# 
#
# We now examine the Khan data set, which consists of a number of tissue
# samples corresponding to four distinct types of small round blue cell tumors.
# For each tissue sample, gene expression measurements are available.
# The data set consists of training data, xtrain and ytrain, and testing data,
# xtest and ytest.

# We examine the dimension of the data:
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
# This data set consists of expression measurements for 2,308 genes.
# The training and test sets consist of 63 and 20 observations respectively.
table(Khan$ytrain)
table(Khan$ytest)
# We will use a support vector approach to predict cancer subtype using gene
# expression measurements. In this data set, there are a very large number
# of features relative to the number of observations. This suggests that we
# should use a linear kernel, because the additional flexibility that will result
# from using a polynomial or radial kernel is unnecessary.
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear",cost=10)
summary(out)
table(dat$y, out$fitted)
# We see that there are no training errors. In fact, this is not surprising,
# because the large number of variables relative to the number of observations
# implies that it is easy to find hyperplanes that fully separate the classes.We
# are most interested not in the support vector classifier’s performance on the
# training observations, but rather its performance on the test observations.
dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(dat.te$y,pred.te)
# We see that using cost=10 yields two test set errors on this data.
