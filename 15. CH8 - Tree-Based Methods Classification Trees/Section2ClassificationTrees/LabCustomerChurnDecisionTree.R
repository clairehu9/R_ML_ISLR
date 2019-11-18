###Set up###
rm(list=ls())
setwd('C://Users//liyan//OneDrive - William & Mary//W&M MSBA Courses Doc//BUAD 5082 ML2//TreeBasedMethods')
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
needed <- c('rpart','rattle','C50','pROC') # May have trouble installing rattle
installIfAbsentAndLoad(needed)
#rpart is the recursive partitioning package
#the fancyRpartPlot and asRules functions at the end of this script are in the rattle package

###get data and explore###
###to predict customer's propensity of churning
churndata<-read.table("churndata.csv",header=T,sep=",")
names(churndata)
str(churndata) 
summary(churndata)
par(mfrow=c(4, 4))                 #tell R we want a 4x4 grid of plots on the same screen
for(i in c(2,4:17)) {hist(churndata[,i], xlab=names(churndata)[i],main=names(churndata)[i])}
par(mfrow=c(1, 1)) 

#preprocessing
###transform variables and deal with missing values###
churndata$area<-factor(churndata$area)
churndata<-na.omit(churndata)

###partition data into training, validate and test subsets (60/20/20)###
set.seed(527)
nobs <- nrow(churndata) #3333

#set indexes
trainrows <- sample(nobs, 0.6* nobs) #1999
validaterows <- sample(setdiff(seq_len(nobs), trainrows), 0.2* nobs) 
testrows <- setdiff(setdiff(seq_len(nobs), trainrows), validaterows)

train<-churndata[trainrows,]
validate<-churndata[validaterows,]
test<-churndata[testrows,]

#To build a maximal classification tree
###create and examine classification model with cp=0, minsplit=2,minbucket=1 (we'll prune the tree later) ###
#method =anova in the regression setting
rpart<-rpart(churn ~ .,data=train, method="class",parms=list(split="information"),control=rpart.control(usesurrogate=0, maxsurrogate=0,cp=0, minsplit=2,minbucket=1))
#minsplit the min number of observations that must exist in a node in order for a split to be attempted
#cp stands for complexity parameter. The main role of this parameter is to save computing time by pruning off splits that are obviously not worthwhile.
#cp is the amount by which splitting that node improve the relative error.
#you could think of it as the minimum benefit that a split must add to the tree.
#maxsurrogate the number of surrogate splits retained in the output 
#usesurrogate: 0 means display only 

print(rpart)
fancyRpartPlot(rpart, main="Customer Churn Prediction Model")

#evaluate on the training dataset
predict <- predict(rpart, newdata=train, type="class")
table(train$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(train$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))

###evaluate predictive power using validate dataset###
predict <- predict(rpart, newdata=validate, type="class")
table(validate$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(validate$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))

###prune classification tree###
##the more splitting you have, the more likelihood it will overfit, 
#in order to avoid the overfitting, we have to prune the tree by bottom-up techniques to find the right balance between
#the fit to the data and the complexity of the model.

printcp(rpart)                         #open new graphics window 
rpart$cptable                          #gives you the only cptable
xerr<-rpart$cptable[,"xerror"]
minxerr<-which(xerr==min(xerr))
minxerr
plotcp(rpart)

#a simpler model is preferred
mincp<-rpart$cptable[8,"CP"]
mincp
rpart.prune<-prune(rpart,cp=mincp)
#pruning is used to reduce the size of decision trees by removing sections of the tree that provide little power to classifier instances.
predict <- predict(rpart.prune, newdata=validate, type="class")
table(validate$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(validate$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))


mincp<-rpart$cptable[minxerr,"CP"]
mincp
rpart.prune<-prune(rpart,cp=mincp)
###compare pruned/original models on validate dataset###
rpart.prune$cptable                    #open new graphics window
fancyRpartPlot(rpart.prune, main="Pruned Customer Churn Prediction Model")
predict <- predict(rpart.prune, newdata=validate, type="class")
table(validate$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(validate$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))


###evaluate predictive power using test dataset###
asRules(rpart.prune)
predict <- predict(rpart.prune, newdata=test, type="class")
mytable <- table(test$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(test$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))
(correct_pred <- sum(diag(mytable))/sum(mytable))

#roc curve
#ROC curve is commonly used to characterize the sensitivity/specificity tradeoffs for a binary classifier
predict.prob <- predict(rpart.prune, newdata=test, type="prob")
roc.plot(as.integer(as.numeric(test[,1]))-1,predict.prob[,2], 
         main="ROC Curve")
#the second arguement is the probability prediction on the interval [0,1]
#The value under the curve is viewed as a measure of a forecast's accuracy
aucc <- roc.area(as.integer(as.numeric(test[,1]))-1,predict.prob[,2])
print(aucc$A)
#Auc value of 0.7 and higher would usually be considered a good model. 

#specify a loss matrix 
rpart.loss<-rpart(churn ~ .,data=train, method="class",parms=list(split="information"),control=rpart.control(usesurrogate=0,
                      maxsurrogate=0,cp=0,minbucket=1,minsplit=2,loss=matrix(c(0,1,10,0), byrow=TRUE, nrow=2)))
#in a list of form: TN, FP(type 1), FN(type 2), TP
#The loss matrix records relative weights for two-class cases only which refers back to binary splitting technique that we emphasize in the whole classification setting.
#In this example, we provide 2x2 matrix with the 0’s for sensitivity and specifity.
#Here we have considered that the cost of mis-classifying a positive example as a negative observation as 1 unit.
#and cost of mis-classifying a negative example as positive as 10 units.
#FP: its that we are predicting that the customer is going to churn but he does not.
#FN:Falsely say that he does not churn where he actually churns
#it is more important to reduce type 2 error in this case to have a better accuracy of not falsely predicting the outcome
#Therefore we applies more weights to the type 2 error. 

##Extra model##

tree.c5 <- C5.0(churn~., data = train, control = C5.0Control(winnow = TRUE)) #whether to perform feature selection
# The winnowing process is removing predictors that can improve the accuracy of the model
predict.c5 <- predict(tree.c5, test)
mytable <- table(test$churn, predict.c5)
(classification_error <- sum(diag(mytable))/sum(mytable))


