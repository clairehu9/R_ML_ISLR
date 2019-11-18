# MAKE SURE TO HAVE
# churndata.csv

# Set up
rm(list=ls()) # MOST IMPORTANT PART
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
needed <- c('rpart', 'rattle') 
# rpart is the recursice partitioning package for both entropy and Gini Index  
# rattle is a visualization package, where fancyRpartPlot and asRules are taken from
installIfAbsentAndLoad(needed)

# read the file churndata.csv & set working directory
# name it churndata
churndata<-read.table("churndata.csv",header=T,sep=",")

# transform variables and deal with missing values
churndata$area<-factor(churndata$area)
churndata<-na.omit(churndata)

# split the data into training, validate and test subsets (80/20)
set.seed(8) # BEST TEAM
nobs <- nrow(churndata) #number of rows in the data
trainrows <- sample(nobs, 0.8* nobs) 
testrows <- setdiff(seq_len(nobs), trainrows)
train<-churndata[trainrows,]
test<-churndata[testrows,]

# create and examine classification model with cp=0, minbucket=1, minsplit=2
# do not prune the tree yet
# hint* the method is class and the split is information
# leave everything else as default
rpart<-rpart(churn ~ .,data=train, method="class",
             parms=list(split="information"),
             control=rpart.control(usesurrogate=0, maxsurrogate=0,
                                   cp=0, minsplit=2,minbucket=1))


# time to be 'fancy' and predict with the train data set
# create a table to see the train data to make sure the full model is using all the available data
fancyRpartPlot(rpart, main="Customer Churn Prediction Model")
predict <- predict(rpart, newdata=train, type="class")
table(train$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(train$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))

# prune classification tree
# hint* Remember the previous presentation and the one standard deviation rule
# so locate the lowest cross validation error, and find the new mincp needed to prune
rpart$cptable
xerr<-rpart$cptable[,"xerror"]
minxerr<-which(xerr==min(xerr))
minxerr
mincp<-rpart$cptable[6,"CP"]
mincp
rpart.prune<-prune(rpart,cp=mincp)

# compare pruned/original models on test set
# hint* plot the pruned data 'fancy'-like, then
# create a table to see the accuracy of the test data set
rpart.prune$cptable
fancyRpartPlot(rpart.prune, main="Pruned Customer Churn Prediction Model")
predict <- predict(rpart.prune, newdata=test, type="class")
table(test$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(test$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))

# \"> c"/