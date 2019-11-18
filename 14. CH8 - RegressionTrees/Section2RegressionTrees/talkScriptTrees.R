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
needed <- c('rpart','rattle') # May have trouble installing rattle
installIfAbsentAndLoad(needed)

###Discard unwanted columns

weather<-read.table("myweatherdata.csv",sep=",",header=T)
names(weather)
discardcols <- names(weather) %in% c("Date", "Location","RISK_MM")
weather<- weather[!discardcols]
names(weather)

###Build 90% training vector of indices and 10% test data frame named weather.test

set.seed(527)
train<-sample(1:nrow(weather), .9*nrow(weather))
weather.test<-weather[-train,]

###Build rpart decision tree

mymodel <- rpart(Sunshine ~ .,data=weather[train,],method="anova", 
                 control=rpart.control(minsplit=20, minbucket = 7, cp=0.01))

###Various info and graphic displays

print(mymodel)
plot(mymodel)
text(mymodel,pretty=0)
fancyRpartPlot(mymodel, main="Decision Tree") # Gradiant refers to strength of prediction
asRules(mymodel)
printcp(mymodel)
plotcp(mymodel)
xerr<-mymodel$cptable[,"xerror"]
minxerr<-which.min(xerr)
minxerr
mincp<-mymodel$cptable[4,"CP"] # chosen b/c stand dev
mincp
mymodel.prune<-prune(mymodel,cp=mincp)
mymodel.prune$cptable
fancyRpartPlot(mymodel.prune, main="Decision Tree With Minimum C.V. Error")
asRules(mymodel.prune)

###Test model

yhat=predict(mymodel.prune,newdata=weather.test)
test=weather[-train,"Sunshine"]
plot(yhat,test) # Why are they lumped together into distinct groups?
firsterror <- mean((yhat-test)^2)
firsterror

###Building a maximal model

mymodel<-rpart(Sunshine~.,data=weather[train,],method="anova", control=rpart.control(cp=0,minbucket=1,minsplit=2))
fancyRpartPlot(mymodel, main="Maximal Decision Tree")
print(mymodel$cptable)
plotcp(mymodel)
grid()

###Pruning the maximal tree to minimize xerror

xerr<-mymodel$cptable[,"xerror"]
minxerr<-which.min(xerr)
minxerr
mincp<-mymodel$cptable[5,"CP"] # 5 is chosen b/c stand dev
mincp
mymodel.prune<-prune(mymodel,cp=mincp)
mymodel.prune$cptable
fancyRpartPlot(mymodel.prune, main="Decision Tree With Minimum C.V. Error")
asRules(mymodel.prune)

###Predicting on test set

yhat=predict(mymodel.prune,newdata=weather.test)
test=weather[-train,"Sunshine"]
plot(yhat,test) # Why are they lumped together into distinct groups?
maxerror <- mean((yhat-test)^2)
maxerror
