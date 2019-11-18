rm(list=ls())
###############################
########Functions##############
###############################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
################################
### Load required packages #####
################################
needed <- c("MASS","rpart", "rattle")  
installIfAbsentAndLoad(needed)


#Set the Seed
set.seed(527)


###################################### Fit Regression Tree ###############################################

#omit NA
Boston <- na.omit(Boston)

#Build 90% training vector of indices and 10% test data frame
train<-sample(1:nrow(Boston), .9*nrow(Boston))


#Build rpart Decision Tree
mymodel <- rpart(medv ~ .,data=Boston[train,],method="anova",
                 parms=list(split="information"),control=rpart.control(minsplit=20, minbucket=7, cp=0.01))


#use regular plot
plot(mymodel)
text(mymodel,pretty=0)


#using FancyRpartPlot function
fancyRpartPlot(mymodel, main="Fancy Tree")

#The variable lstat measures the percentage of individuals with lower socioeconomic status.
#The tree indicates that lower values of lstat correspond to more expensive houses.
#The tree predicts a median house price of $46, 400 for larger homes in suburbs 
#in which residents have high socioeconomic status (rm>=7.437 and lstat<9.715).


#visualizing the progression of the CP values.
plotcp(mymodel)
#CP is measure of complexity, the larger the number, the simpler.

#open new graphics table
printcp(mymodel) 

###################################Create Maximal Tree Model##################################################

#Build rpart decision tree with cp=0, minsplit=2, minbucket=1 
mymodel_max<-rpart(medv ~ .,data=Boston[train,],method="anova",parms=list(split="information"),
                   control=rpart.control(cp=0,minbucket=1,minsplit=2))

#visualizing the progression of the CP values.
plotcp(mymodel_max)
fancyRpartPlot(mymodel_max, main="Maximum Fancy Tree")

#Disadvantage of Maximal Tree: Overtrained model, high variance, low bias
#Therefore, need to decide boundary by pruning the tree.


############################################Prune the Tree####################################################

#Decide CP
mymodel_max$cptable
xerr<-mymodel_max$cptable[,"xerror"]
(minxerr<-which.min(xerr))
 mincp<-mymodel_max$cptable[8,"CP"] #0.312
mymodel_max.prune<-prune(mymodel_max,cp=mincp)
mymodel_max.prune$cptable

#Display tree after prune
fancyRpartPlot(mymodel_max.prune, main="Tree With Minimum C.V. Error")


################################Make predictions on the test set##############################################

yhat <- predict(mymodel_max.prune,newdata=Boston[-train,])
Boston.test<-Boston[-train,"medv"]

plot(Boston.test, yhat, type='p')


#Calculate MSE
(mse <- (mean((yhat-Boston.test)^2))) 


#Calculate range within 95% Confidence Interval
sd <- sqrt(mse)
(lb <- mean(Boston$medv - 2 * sd))
(ub <- mean(Boston$medv + 2 * sd))



