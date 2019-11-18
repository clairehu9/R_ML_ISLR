rm(list=ls())

installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
needed <- c('ISLR', 'e1071')      
installIfAbsentAndLoad(needed)

#####################
#  Part a ##
#####################
set.seed(5082)
n = dim(OJ)[1]
train_inds = sample(1:n, 800)
test_inds = (1:n)[-train_inds]
#Create test and train data
train_y = OJ$Purchase[train_inds]
train_x = OJ[2:18][train_inds, ]
test_y = OJ$Purchase[test_inds]
test_x = OJ[2:18][test_inds, ]

#Turn this into usable data for the svm function
traindat <- data.frame(x = train_x, y = train_y)
testdat <- data.frame(x = test_x, y = test_y)

#####################
#  Part b ##
#####################
# A suport vector classifier (linear kernel but a particular cost = 1)
svmfitTrain <- svm(y ~ ., data = traindat, 
                   kernel="linear", 
                   cost = 1, 
                   scale = TRUE)
#Summary of SVM
summary(svmfitTrain)

# The results of the SVMTrain indicate that there were 446 support vectors- 223 for the
# CH class and 223 for the MM class. This is a high number (considering we're in a training
# set of 800 points), which indicates that the data is probably very close together and often
# misclassified since support vectors are near the margins or misclassified.

#####################
#  Part c ##
#####################
trainpred <- predict(svmfitTrain, traindat)
trainError <- table( actual = traindat$y, predicted = trainpred)
print(paste("The training error rate is", (trainError[1, 2] + trainError[2, 1])/sum(trainError)))

testpred <- predict(svmfitTrain, testdat) #use old svmfitTrain and predict on testdata
(testErrorTable <- table(actual = testdat$y, predicted = testpred))
svclassifier.Error.rate <- (testErrorTable[1, 2] + testErrorTable[2, 1])/sum(testErrorTable)
print(paste("The test error rate using a support vector classifier is", svclassifier.Error.rate))

############################
#  Parts d and e ##
############################
# Linear kernel with optimized cost
tune.out <- tune(svm, y ~ ., data = traindat, kernel="linear", 
                 ranges=list(cost=c(0.01, 0.05, 0.1, 0.5, 1, 5)))
bestmod <- tune.out$best.model
summary(bestmod)
ypred <- predict(bestmod, traindat)
(trainTuneOptimal <- table(actual = traindat$y, predicted = ypred))
print(paste("The training error rate with a linear kernel after being tuned is", (trainTuneOptimal[1, 2] + trainTuneOptimal[2, 1])/sum(trainTuneOptimal)))

ypred <- predict(bestmod, testdat)
(testTuneOptimal <- table(actual = testdat$y, predicted = ypred))
linear.Error.rate <- (testTuneOptimal[1, 2] + testTuneOptimal[2, 1])/sum(testTuneOptimal)
print(paste("The test error rate with a linear kernel after being tuned is", linear.Error.rate))
#####################
#  Part f ##
#####################
# Radial kernel
tune.out <- tune(svm, y ~ ., data = traindat, kernel="radial", 
                 ranges=list(cost=c(0.01, 0.05, 0.1, 0.5, 1, 5),
                             gamma=c(0.001, 0.01, 1, 3, 5)))
bestmod <- tune.out$best.model
summary(bestmod)
ypred <- predict(bestmod, traindat)
trainTuneOptimal <- table(actual = traindat$y, predicted = ypred)
print(paste("The training error rate with a radial kernel after being tuned is", (trainTuneOptimal[1, 2] + trainTuneOptimal[2, 1])/sum(trainTuneOptimal)))

ypred <- predict(bestmod, testdat)
(testTuneOptimal <- table( actual = testdat$y, predicted = ypred))
radial.Error.rate <- (testTuneOptimal[1, 2] + testTuneOptimal[2, 1])/sum(testTuneOptimal)
print(paste("The test error rate with a radial kernel after being tuned is", radial.Error.rate))

#####################
#  Part g ##
#####################
# Polynomial kernel
tune.out <- tune(svm, y ~ ., data = traindat, kernel="polynomial", 
                 ranges=list(cost=c(0.01, 0.05, 0.1, 0.5, 1, 5),
                             degree=2:5))
bestmod <- tune.out$best.model
summary(bestmod)
ypred <- predict(bestmod, traindat)
trainTuneOptimal <- table( actual = traindat$y, predicted = ypred)
print(paste("The training error rate using a polynomial kernel after being tuned is", (trainTuneOptimal[1, 2] + trainTuneOptimal[2, 1])/sum(trainTuneOptimal)))

ypred <- predict(bestmod, testdat)
(testTuneOptimal <- table( actual = testdat$y, predicted = ypred))
poly.Error.rate <- (testTuneOptimal[1, 2] + testTuneOptimal[2, 1])/sum(testTuneOptimal)
print(paste("The test error rate using a polynomial kernel after being tuned is", poly.Error.rate))


#####################
#  Part h ##
#####################
data.frame(svclassifier.Error.rate, 
           linear.Error.rate, 
           radial.Error.rate, 
           poly.Error.rate)
# The best approach appears to be either the linear kernal 
# with cost set to 0.05 or the radial kernel with cost set 
# to 5 and gamma set to 0.01. Both models had the same test 
# error rates (not that unusual...they both made 43
# prediction errors - their type 1/2 error rates just
# differed slightly). Since the linear model is the simpler
# model, I would choose it over the model with the radial
# kernel.


