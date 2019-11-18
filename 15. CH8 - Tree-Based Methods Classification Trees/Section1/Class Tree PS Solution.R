###########################################
#Classification Tree Problem Set Solutions#
###########################################

#############################
# Question 1.a, Preparation #
#############################
rm(list = ls())

#Import Package

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c("pROC","rpart", "rattle")  
installIfAbsentAndLoad(needed)

#Read in Full Data
full_data <- read.csv("myWeatherData.csv")

##############################
# Question 1.b   #############
##############################

#Omit NA's and Columns
omit_data <- na.omit(full_data)
head(omit_data)
omit_data <- omit_data[-c(1,2, 23)]

#Set Seed and 90/10 Training Test Split
set.seed(525600)
nobs <- nrow(omit_data)
train <- sample(1:nobs, .9*nobs)
test <- setdiff(1:nobs, train)
train_data <- omit_data[train,]
test_data <- omit_data[test,]

#Build a Maximal Tree
max_tree <- rpart(RainTomorrow ~ .,
                  data = train_data,
                  method = 'class',
                  parms = list(split = "information"),
                  control = rpart.control(minsplit = 2, minbucket = 1, cp = 0))
#Prune it Back

min.xerror <-  which.min(max_tree$cptable[,'xerror'])
min.xerror
max_tree$cptable #Row 2 has xerror of .76 and std of .1; no row prior to it is within 1 std, so use it)
best_cp <- max_tree$cptable[2,'CP']
prune_tree <- prune(max_tree, cp = best_cp)

#Visualize It and print Rules
par(mfrow=c(1,1))
fancyRpartPlot(prune_tree)
asRules(prune_tree)

#Evaluate Test Set
prune.preds <- predict(prune_tree, newdata = test_data, type = 'class')
table(test_data$RainTomorrow, prune.preds,dnn=c("Actual", "Predicted"))
round(100*table(test_data$RainTomorrow, prune.preds,dnn=c("% Actual", "% Predicted"))/length(prune.preds))

#Generate ROC Curve
prune.probs <- predict(prune_tree, newdata = test_data)
prune.roc <- roc(as.numeric(test_data$RainTomorrow), prune.probs[,2])
plot(prune.roc, col = 'blue', lty = 1)

#Default Tree
default_tree <- rpart(RainTomorrow ~.,
                      data = train_data,
                      method = 'class')

#Produce Probabilities and ROC for Default and Maximal
max.probs <- predict(max_tree, newdata = test_data)
max.roc <- roc(as.numeric(test_data$RainTomorrow), max.probs[,2])
par(new = T)
plot(max.roc, col = 'red', lty = 2)

def.probs <- predict(default_tree, newdata = test_data)
def.roc <- roc(as.numeric(test_data$RainTomorrow), def.probs[,2])
par(new = T)
plot(def.roc, col = 'green', lty = 3)

legend('bottomleft',c('Pruned','Maximal','Default'), lty = c(1,2,3), col = c('blue','red','green'))

#This really shows the predictive weakness of trees;
#Pruning them can still cause errors when validating across data sets due to sensitivity to the data
#And the relative presence of different categories.
#Not great at capturing variation

##########################
# Question 1.c    ########
##########################

## 1.c.i ##
#First, build a default model without removing NA's
full_data = full_data[-c(1,2,23)]
set.seed(572)
nobs <- nrow(full_data)
train <- sample(1:nobs, .9*nobs)
test <- setdiff(1:nobs, train)
train_data2 <- full_data[train,]
test_data2 <- full_data[test,]

# Max Tree
na_tree <- rpart(RainTomorrow ~ .,
                 data = train_data2,
                 method = 'class',
                 parms = list(split = 'information'),
                 control = rpart.control(usesurrogate = 1, maxsurrogate = 2, minsplit = 2, minbucket = 1, cp = 0)) 

#Prune
min.xerror <- which.min(na_tree$cptable[,'xerror'])
min.xerror
na_tree$cptable
best_cp <- na_tree$cptable[2,'CP']
na_tree <- prune(na_tree, cp = best_cp)

## 1.c.ii ##

#Predict Both NA Tree and Default Non-NA on SecondTest Set
na.preds <- predict(na_tree, newdata = test_data2, type = 'class')
prune.preds <- predict(prune_tree, newdata = test_data2, type = 'class')
round(100*table(test_data2$RainTomorrow, na.preds,dnn=c("% Actual", "% Predicted"))/length(na.preds))
round(100*table(test_data2$RainTomorrow, prune.preds,dnn=c("% Actual", "% Predicted"))/length(prune.preds))

#Visualize the trees
par(mfrow = c(2,1))
fancyRpartPlot(na_tree, main = 'NA Tree')
fancyRpartPlot(prune_tree, main = 'Prune Tree')

# We do see a loss here in predictive power, which may come from
# Variation in the data used to create our trees and the given split
# It is likely this more so than a raw predicitve issue, as our NA Tree is slightly larger
# We would want to use surrogates if our training data has many many NA's or if we 
# believe that data we want to predict on is likely to have NA's

## 1.c.iii ##
par(mfrow = c(1,1))
prior_tree <- rpart(RainTomorrow ~ .,
                    data = train_data,
                    method = 'class',
                    parms = list(split = 'information', prior = c(.3,.7)))
fancyRpartPlot(prior_tree)
#Key argument here is parms = list(...prior = c(.3,.7))

## 1.c.iv ##
loss.matrix <- matrix(c(0,1,42,0), byrow = T, nrow = 2)
loss_tree <- rpart(RainTomorrow ~ .,
                   data = train_data,
                   method = 'class',
                   parms = list(split = 'information', loss = loss.matrix))
fancyRpartPlot(loss_tree)    
#Key argument here is parm = list(...loss = loss.matrix)
#Your loss matrix must have the [2,1] spot 42 times larger than the [1,2] spot



###################################
## Question II ####################
###################################

#A. Tree Terminology
  #i. True, it is greedy because it doesn't go backwards, recursive because it restarts at every node, and binary since it splits in 2 directions
  #ii. True, since that makes the probability work well. Caveat, we want high purity on all data, not just training data. Perfect training purity is likely overfit
  #iii. True, you can use asRules() or print() on the tree to produce them in R
  #iv. False, they are called Maximal Trees

#B. Error Metrics
  #i. False; we are predicting classes, not values, and therefore we cannot use RSS like regression
  #ii. True, they both  measure homogeneity and track each other closely
  #iii. False, a value of .5 means its very heterogeneous (mixed) which minimizes predictive power. We want 0 or 1
  #iv. True, we use CP in the pruning statement to tell the tree how far to go back. Cross-validation error is how we select the best CP.

#C. R implementation
  #i. False, we use method = 'class'. 'binary' should sound familiar however (logistic regression)
  #ii. True! It may be worth experimenting using gini to see if you get a different or better tree (usually you won't)
  #iii. Close, but false. We need the values to add to 1, so we want priors = c(.5,.5)
  #iv. False again, this matrix penalizes type 2 error (remember it easily since it occurs in row2)

  