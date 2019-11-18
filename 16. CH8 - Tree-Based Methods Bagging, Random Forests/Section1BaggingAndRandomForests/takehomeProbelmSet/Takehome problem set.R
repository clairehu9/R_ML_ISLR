rm(list=ls())
require(randomForest)
###Get the data
churndata<-read.table("churndata.csv",sep=",",header=T)
###Clean up, change area code to a factor
data <- na.omit(churndata)
str(data)
data$area<-factor(data$area)




###Create training and test sets
nobs <- nrow(data)
set.seed(527)
train <- sample(nobs, 0.7*nobs)




###Grow a 500-tree forest, with mtry=4   
#rf.original
rf.original <- randomForest(formula=churn ~ .,data=data[train,],ntree=500, mtry=4,
                   importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)
head(rf.original$predicted,25)
rf.original


###too much no, we want more yes
num.of.yes<-length(which(churndata$churn=='Yes'))
num.of.yes


##use samplesize()to deal with unbalanced data
##here we use sample size =100, meaning we take same ampunt of yes and no from training set during the bagging process
set.seed(527)
#rf.reshape
rf.reshape <- randomForest(formula=churn ~ .,data=data[train,], ntree=500,mtry=4, 
                           importance=TRUE, na.action=na.roughfix, replace=TRUE, sampsize=c(50,50))
rf.reshape


##pruning the forest
###decide ntree,mtry 
oob.err<-rep(0,16)
min.ntree<-rep(0,16)
accuracy<-rep(0,16)
for(mtry in 1:16){
  rf_mtry <- randomForest(formula=churn ~ .,data=data[train,], ntree=400, mtry=mtry,
                          importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=TRUE)
  accuracy[mtry]=1-rf_mtry$err.rate[which(rf_mtry$err.rate[,"OOB"]== min(rf_mtry$err.rate[,"OOB"]))[1]]
  min.ntree[mtry]=which(rf_mtry$err.rate[,"OOB"]== min(rf_mtry$err.rate[,"OOB"]))[1]
  oob.err[mtry]=rf_mtry$err.rate[which(rf_mtry$err.rate[,"OOB"]== min(rf_mtry$err.rate[,"OOB"]))[1]]
  
  
}
matplot(1:mtry,c(oob.err),pch=19,col="blue",type="b",ylab="oob error")
##find the best mtry by using the one standard deviation rule
min(oob.err)+sd(oob.err)
##find ntree from the result table 
result<-cbind(oob.err,min.ntree)
result



###Calculate the area under the ROC curve and confidence interval for this value
require(pROC)
roc(rf.original$y, as.numeric(rf.original$predicted))
ci.auc(rf.original$y, as.numeric(rf.original$predicted))
###Plot the OOB ROC curve. pick(0.6,0.4)
require(verification)
aucc <- roc.area(as.integer(as.factor(data[train,"churn"]))-1,rf.original$votes[,2])$A
roc.plot(as.integer(as.factor(data[train,"churn"]))-1,rf.original$votes[,2], 
         main="OOB ROC Curve for the Random Forest")





###rebuild the forest by changing the vote cutoff,ntree,mtry,sample size
###Change vote cutoff to reduce Type II Error Rate (at the expense of the Type I Error Rate)
set.seed(527)
rf.final <- randomForest(formula=churn ~ .,data=data[train,], ntree=127,mtry=7, 
                         importance=TRUE, na.action=na.roughfix, replace=TRUE,
                         sampsize=c(50,50),cutoff=c(0.6,0.4))
rf.final







###finally
###List the importance of the variables.
rn <- round(importance(rf.final), 2)
rn[order(rn[,3], decreasing=TRUE),]
###Display a chart of Variable Importance
varImpPlot(rf.final, main="Variable Importance in the Random Forest")






