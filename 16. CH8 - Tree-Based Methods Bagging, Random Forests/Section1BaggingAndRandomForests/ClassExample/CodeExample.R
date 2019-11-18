rm(list=ls())
require(randomForest)
weather<-read.table("myweatherdata.csv",sep=",",header=T)
dim(weather)
###Discard unwanted columns
names(weather)
discardcols <- names(weather) %in% c("Date", "Location","RISK_MM")
weather<- weather[!discardcols]
names(weather)
str(weather)


###Build 70% training vector of indices and 30% test data frame named weather.test
set.seed(42)
train<-sample(1:nrow(weather), 0.7*nrow(weather))
weather.test<-na.omit(weather[-train,])  #get our test set




###Grow a Random Forest with ntree=500, mtry=4(want ntree as big as possible)
set.seed(42)
?randomForest
rf.raw <- randomForest(RainTomorrow ~ .,data=weather[train,], ntree=500,mtry=4,
                   importance=TRUE, na.action=na.roughfix,replace=TRUE)

rf.raw

# ●	ntree: number of trees of the model to grow, don’t set this number too smal
# ●	mtry: number of variables randomly sampled as candidates at each split. 
#   Default value for classification is sqrt(# of predictors), for regression is (# of predictors) / 3
# For every split in every tree you pick 4 random predictors
#
# OOB error rate = 14.45% 
# Type 2 Error is high, Predit No Rain but Actual Raning is 29

###decide the number of mtry,ntree
oob.err<-rep(0,20)
min.ntree<-rep(0,20)
for(mtry in 1:20){
  rf_mtry <- randomForest(formula=RainTomorrow ~ .,data=weather[train,], ntree=500, mtry=mtry,
                      importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=TRUE)
  min.ntree[mtry]=which(rf_mtry$err.rate[,"OOB"]== min(rf_mtry$err.rate[,"OOB"]))[1]
  oob.err[mtry]=rf_mtry$err.rate[which(rf_mtry$err.rate[,"OOB"]== min(rf_mtry$err.rate[,"OOB"]))[1]]
}
matplot(1:mtry,c(oob.err),pch=19,col="blue",type="b",ylab="OOB error",xlab = "mtry")
#one standard deciation rule 
sd(oob.err)+min(oob.err)

result<-cbind(oob.err,min.ntree)
names(result)<-c('OOB error','ntree')
result

# one standard deviation rule
min(oob.err) # the minimum out of bag error
sd(oob.err) + min(oob.err)

# you want decorrelated trees, how do you decorrelate them more? By choosing fewer and fewer variables
# to consider at each split
# the whole point is to overtrain your trees as individuals (and in completely different ways), and they 
# will combined, minimize variance




###Calculate the area under the ROC curve and confidence interval for this value
require(pROC)
roc(rf.raw$y, as.numeric(rf.raw$predicted))           # AUC = 0.6277
ci.auc(rf.raw$y, as.numeric(rf.raw$predicted))        # the confidence interval (CI) of an area under the curve (AUC)
###Plot the OOB ROC curve.
require(verification)
aucc <- roc.area(as.integer(as.factor(weather[train,"RainTomorrow"]))-1,rf.raw$votes[,2])$A
roc.plot(as.integer(as.factor(weather[train,"RainTomorrow"]))-1,rf.raw$votes[,2], 
         main="OOB ROC Curve for the Random Forest")




###rebuild the forest by changing the vote cutoff,ntree,mtry,sample size
###Change vote cutoff to reduce Type II Error Rate (at the expense of the Type I Error Rate increase)
set.seed(42)
rf.final <- randomForest(RainTomorrow ~ .,data=weather[train,], ntree=108,mtry=6, 
                   importance=TRUE, na.action=na.roughfix, replace=TRUE,cutoff=c(0.6,0.4))
rf.final
# if over 40% of the trees classify yes it will go to yes (changing cutoff num)

# this acts as a comparison to what happens when you dont use a diff cutoff point
rf.nocutoff <- randomForest(RainTomorrow ~ .,data=weather[train,], ntree=108,mtry=6, 
                         importance=TRUE, na.action=na.roughfix, replace=TRUE)
rf.nocutoff




###List the importance of the variables.
rn <- round(importance(rf.final), 2)
rn[order(rn[,3], decreasing=TRUE),]
###Display a chart of Variable Importance
varImpPlot(rf.final, main="Variable Importance in the Random Forest")
