rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c("rpart", "rattle", "ISLR")  
installIfAbsentAndLoad(needed)


###################################################
###  Step 1  #######
#- Set the seed to 527 
#- Omit the Na of the dataset
#- Build 80% training vector of indices named train
#- Build 20% test data frame named hitters.test
###################################################

set.seed(527)
Hitters <- na.omit(Hitters)
train<- sample(1:nrow(Hitters), .8*nrow(Hitters))
hitters.test<-Hitters[-train,]

###################################################
###  Step 2 Build and Plot The Maximal Tree #######
#- Build a maximal Spanning tree model with data  using the following control parameters:
#- control=rpart.control(cp=0,minbucket=1,minsplit=2))
#- Use fancyRpartPlot(modelname, main="Maximal Decision Tree") to plot the maximal tree
###################################################

mymodel<-rpart(Salary~.,data=Hitters[train,],method="anova",
               parms=list(split="information"),
               control=rpart.control(cp=0,minbucket=1,minsplit=2))

fancyRpartPlot(mymodel, main="Maximal Decision Tree")

###################################################
###################################################
###  Step 3 Prune The Maximal Tree  ###############
# -Create a pruned tree model using the one SD rule
###################################################
###################################################

mymodel$cptable
xerr<-mymodel$cptable[,"xerror"]
(minxerr<-which.min(xerr))
mincp<-mymodel$cptable[3,"CP"] #use the minerror as 3 according to the one sd rule

mymodel.prune<-prune(mymodel,cp=mincp)
fancyRpartPlot(mymodel.prune, main="Decision Tree With Minimum C.V. Error")
asRules(mymodel.prune)


####################################################
####################################################
###  Step 4 Predict The Test Set  #######
#- predict the salary from your pruned model
#- plot the predictions againt the test
#- calculate the mse of the predicted model
#- calculate the upper and lower bounds of a 95% CI
####################################################
####################################################

yhat=predict(mymodel.prune,newdata=hitters.test)
hittersNew.test<-hitters.test$Salary
plot(hittersNew.test, yhat, type='p')
(mse <- (mean((yhat-hittersNew.test)^2))) #calculate MSE
(sd <- sqrt(mse))
(lb <- mean(Hitters$Salary - 2 * sd))
(ub <- mean(Hitters$Salary + 2 * sd))







