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
needed <- c('randomForest', 'pROC', 'verification', 'rpart', 'ISLR')
installIfAbsentAndLoad(needed)


###Get the data
###Get the data
data <- Smarket
str(data)
head(data)
data <- data[-8] # get rid of variable 'Today', as it's perfectly correlated with our response value

###Create training and test sets
nobs <- nrow(data)
set.seed(5082)
train <- sample(nobs, 0.7*nobs)
test <- setdiff(1:nobs, train)

###Grow a 500-tree forest
rf <- randomForest(formula=Direction ~ .,data=data[train,],ntree=500, mtry=3,
                   importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)
head(rf$predicted,25)
###Display Variable Importance   
importance(rf)[order(importance(rf)[,"MeanDecreaseAccuracy"], decreasing=T),]
###Display a chart of Variable Importance
varImpPlot(rf, main="Variable Importance in the Random Forest")

###Examine Error Rates for the Trees
head(rf$err.rate)
plot(rf, main="Error Rates for Random Forest")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)
min.err <- min(rf$err.rate[,"OOB"])
min.err.idx <- which(rf$err.rate[,"OOB"]== min.err)
min.err.idx
rf$err.rate[min.err.idx[1],]

###Rebuild the forest with the number of trees that minimizes the OOB error rate - use the first one if there are more than one
set.seed(5082)
rf <- randomForest(formula=Direction ~ .,data=data[train,],ntree= min.err.idx[1], mtry=3,
                   importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)
rf # OOB error rate is 48.23%
###Look at voting info for each observatoion
head(rf$votes)
rf$confusion #Type I: 0.565; Type II: 0.410

###Plot the OOB ROC curve and calculate AUC. 
aucc <- roc.area(as.integer(as.factor(data[train, "Direction"]))-1,rf$votes[,2])
aucc$A
# AUC for this ROC curve is very close to 0.5, which is very close to naive prediction,
# because it's predicting trend of stock market.  
roc.plot(as.integer(as.factor(data[train,"Direction"]))-1,rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc$A))
title(main="OOB ROC Curve Random Forest Directiondata.csv",
      sub=paste("David Murray", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
# We don't want to change the cutoff point because there's no difference between 
# predicting a false 'Up' and false 'Down'. In both cases, you lose money from your
# invested stocks.

###Evaluate by scoring the training set
prtrain <- predict(rf, newdata=data[train,])
table(data[train,"Direction"], prtrain,dnn=c("Actual", "Predicted"))
round(100* table(data[train,"Direction"], prtrain,dnn=c("% Actual", "% Predicted"))/length(prtrain))
###Evaluate by scoring the test set
prtest <- predict(rf, newdata=na.omit(data[test,]))
table(data[test,"Direction"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(data[test,"Direction"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)
(test.error <- mean(data[test,"Direction"] != prtest))

# The test error rate is even worse than that of the naive predictions. It further suggests
# it's really hard to predict the trend of stock market, or we all can easily 
# make money out of this simple model.
