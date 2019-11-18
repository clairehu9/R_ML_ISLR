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
needed <- c('randomForest', 'pROC', 'verification', 'rpart')
installIfAbsentAndLoad(needed)


###Get the data
churndata<-read.table("churndata.csv",sep=",",header=T)
###Clean up, change area code to a factor
sum(is.na(churndata))
data <- na.omit(churndata)
str(data)
data$area<-factor(data$area)

###Create training and test sets
nobs <- nrow(data)
set.seed(5082)
train <- sample(nobs, 0.7*nobs)


###Grow a 500-tree forest
rf <- randomForest(formula=churn ~ .,data=data[train,],ntree=500, mtry=4,
	importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)

summary(rf)
head(rf$predicted,25)
rf$oob.times

###Display Variable Importance   
importance(rf)[order(importance(rf)[,"MeanDecreaseAccuracy"], decreasing=T),]
importance(rf)[order(importance(rf)[,"MeanDecreaseGini"], decreasing=T),]
###Display a chart of Variable Importance
varImpPlot(rf, main="Variable Importance in the Random Forest")

###Examine Error Rates for the Trees
head(rf$err.rate)
plot(rf, main="Error Rates for Random Forest")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)
rf$confusion
min.err <- min(rf$err.rate[,"OOB"])
min.err.idx <- which(rf$err.rate[,"OOB"]== min.err)
min.err.idx
rf$err.rate[min.err.idx[1],]

###Rebuild the forest with the number of trees that minimizes the OOB error rate - use the first one if there are more than one
set.seed(5082)
rf <- randomForest(formula=churn ~ .,data=data[train,],ntree= min.err.idx[1], mtry=4,
	importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)
###Look at voting info for each observatoion
head(rf$votes)


###Plot the OOB ROC curve and calculate AUC. 
aucc <- roc.area(as.integer(as.factor(data[train, "churn"]))-1,rf$votes[,2])
aucc$A
aucc$p.value                #null hypothesis: aucc=0.5 
roc.plot(as.integer(as.factor(data[train,"churn"]))-1,rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc$A))
title(main="OOB ROC Curve Random Forest churndata.csv",
    sub=paste("David Murray", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
###Evaluate by scoring the training set
prtrain <- predict(rf, newdata=data[train,])
table(data[train,"churn"], prtrain,dnn=c("Actual", "Predicted"))
round(100* table(data[train,"churn"], prtrain,dnn=c("% Actual", "% Predicted"))/length(prtrain))
###Evaluate by scoring the test set
test <- setdiff(1:nobs, train)
prtest <- predict(rf, newdata=na.omit(data[test,]))
table(data[test,"churn"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(data[test,"churn"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)

###Change vote cutoff to reduce Type II Error Rate (at the expense of the Type I Error Rate)
set.seed(527)
rfLowerT2Error <- randomForest(formula=churn ~ .,data=data[train,],ntree=500, mtry=4,
             importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE,cutoff=c(0.8,0.2))
rfLowerT2Error
###Evaluate by scoring the test set
prtestLowerT2Error <- predict(rfLowerT2Error, newdata=na.omit(data[test,]))
table(data[test,"churn"], prtestLowerT2Error ,dnn=c("Actual", "Predicted"))
round(100* table(data[test,"churn"], prtestLowerT2Error ,dnn=c("% Actual", "% Predicted"))/length(prtestLowerT2Error))


###Build a single tree using rpart, prune it, and evaluate it using the same test set
rpart<-rpart(churn ~ .,data=data[test,], method="class",parms=list(split="information"),control=rpart.control(usesurrogate=0, maxsurrogate=0))
xerr<-rpart$cptable[,"xerror"]
minxerr<-which.min(xerr)
mincp<-rpart$cptable[minxerr,"CP"]
###prune classification tree###
rpart.prune<-prune(rpart,cp=mincp)
predictonetree <- predict(rpart.prune, newdata=data[test,], type="class")
mytable<-table(data[test,"churn"], predictonetree ,dnn=c("Actual", "Predicted"))
mytable
round(100*mytable/sum(mytable))
