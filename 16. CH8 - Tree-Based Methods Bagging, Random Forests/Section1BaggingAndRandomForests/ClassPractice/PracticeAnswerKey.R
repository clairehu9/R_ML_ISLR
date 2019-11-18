rm(list=ls())
#download random forest package
require(randomForest)

###Get and clean the data
churndata<-read.table("churndata.csv",sep=",",header=T)
data <- na.omit(churndata)
str(data)
data$area<-factor(data$area)




###Create training and test sets
nobs <- nrow(data)
set.seed(527)
train <- sample(nobs, 0.7*nobs)




###Grow a 500-tree forest, with mtry=4
##pay attention to the OOB error rate and confusion matrix

rf.original <- randomForest(formula=churn ~ .,data=data[train,],ntree=500, mtry=4,
                            importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)
head(rf.original$predicted,25)
rf.original










###Calculate the area under the ROC curve and confidence interval 
require(pROC)
roc(rf.original$y, as.numeric(rf.original$predicted))
ci.auc(rf.original$y, as.numeric(rf.original$predicted))



###Plot the OOB ROC curve. 
###think: what's our preference regarding the Type I/Type II trade error off for this dataset?
##hint: think from the real world business perspective 
require(verification)
aucc <- roc.area(as.integer(as.factor(data[train,"churn"]))-1,rf.original$votes[,2])$A
roc.plot(as.integer(as.factor(data[train,"churn"]))-1,rf.original$votes[,2], 
         main="OOB ROC Curve for the Random Forest")







###rebuild the forest by changing the vote cutoff
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


###Display a chart of Variable Importance for your final model
varImpPlot(rf.final, main="Variable Importance in the Random Forest")







