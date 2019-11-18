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
#pRoc contains roc.plot
#verification contains roc.area
#rpart for building a single tree
needed <- c('randomForest', 'pROC', 'verification', 'rpart', 'ISLR', 'Metrics', 'ggplot2')
installIfAbsentAndLoad(needed)

###Get the data
data <- Boston
str(data)

###Create training and test sets
nobs <- nrow(data)
set.seed(5082)
train <- sample(nobs, 0.7*nobs)
test <- setdiff(1:nobs, train)


###Grow a 500-bagging trees
rf.bag <- randomForest(formula=medv ~ .,data=data[train,],ntree=500, mtry=13,
                   importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=TRUE)
rf.bag
head(rf.bag$predicted,25)
###Display Variable Importance   
importance(rf.bag)[order(importance(rf.bag)[,"%IncMSE"], decreasing=T),]
###Display a chart of Variable Importance
varImpPlot(rf.bag, main="Variable Importance in the Bagging")


###Grow a 500-tree random forest
set.seed(5082)
rf <- randomForest(formula=medv ~ .,data=data[train,],ntree=500, mtry=4,
                   importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=TRUE)

head(rf$predicted,25)
###Display Variable Importance   
importance(rf)[order(importance(rf)[,"%IncMSE"], decreasing=T),]
###Display a chart of Variable Importance
varImpPlot(rf, main="Variable Importance in the Random Forest")

#Bagging model considers all predictors at each split, while random forest model 
# at each split randomly select only 4 predictors as split candidates. It can be seen 
# from the variable importance plots as well. Comparing with the bagging model, 
# there exist smaller gaps between non-important variables and important variables(rm, lstat)
# both in %IncMSE plot and IncNodePurity plot. This supports the idea that random
# forest algorithm will bring more opportunities for non-important predictors to make
# decisions at each split, which results further decrease in variance.

###Examine MSEs for the Trees
head(rf$mse)
plot(rf, main="MSEs for Random Forest")
#plot(rf$rsq, main = "R-Squares for Random Forest")
min.mse <- min(rf$mse)
min.mse.idx <- which(rf$mse == min.mse)
min.mse.idx
rf$mse[min.mse.idx[1]]

###Rebuild the forest with the number of trees that minimizes the OOB error rate - use the first one if there are more than one
set.seed(5082)
rf <- randomForest(formula=medv ~ .,data=data[train,],ntree= min.mse.idx[1], mtry=4,
                   importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=TRUE)

###Evaluate by scoring the training set
prtrain <- predict(rf, newdata=data[train,])
trainMSE <- mse(actual = data[train, 'medv'], predicted = prtrain)

###Evaluate by scoring the test set
prtest <- predict(rf, newdata=data[test,])
testMSE <- mse(actual = data[test, 'medv'], predicted = prtest)

print(trainMSE)
print(rf$mse[length(rf$mse)])
print(testMSE)

# Train MSE 1.90 is very small, comparing to OOB MSE of 9.96 and test MSE of 20.16
# The reason why test MSE is so high is probalby because of randomness, if we do 
# hundreads of iterations, it should be very close to OOB MSE.


# Here I do 100 iterations to justify my explanation above
testMSEs <- c()
trainMSEs <- c()
OOBMSEs <- c()
for (i in 1:100) {
        nobs <- nrow(data)
        train <- sample(nobs, 0.7*nobs)
        test <- setdiff(1:nobs, train)
        rf <- randomForest(formula=medv ~ .,data=data[train,],ntree= min.mse.idx[1], mtry=4,replace=TRUE)
        prtrain <- predict(rf, newdata=data[train, ])
        trainMSEs[i] <- mse(actual = data[train, 'medv'], predicted = prtrain)
        ###Evaluate by scoring the test set
        prtest <- predict(rf, newdata=(data[test,]))
        testMSEs[i] <- mse(actual = data[test, 'medv'], predicted = prtest)
        OOBMSEs[i] <- rf$mse[length(rf$mse)]
}


ggplot(data.frame(trainMSEs, testMSEs), aes(1:100, trainMSEs, colour = 'Train MSE')) +
        geom_line(aes(y = testMSEs, colour = 'Test MSE')) + 
        geom_line(aes(y = OOBMSEs, colour = 'OOB MSE')) +
        geom_line() + 
        labs(title = 'MSE Comparison', 
             x = "Number of Iterations", y = "MSE") +
        theme(legend.title = element_blank())
