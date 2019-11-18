rm(list = ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c("pROC","rpart", "rattle","verification","ISLR")  
installIfAbsentAndLoad(needed)

#######################################
#Data Loading and Manipulation#########
#######################################

my_df <- OJ
summary(my_df)
names(my_df)
my_df <- my_df[!names(my_df) %in% c("StoreID","STORE","Store7")]
head(my_df)

######################################3
#Reserve a Test Set###################
######################################

nrow(my_df)

#That's enough rows, let's do a 90/10 split

set.seed(8) #BestTeam
train <- sample(1:nrow(my_df), .9*nrow(my_df))
test <- setdiff(1:nrow(my_df), train)
train_data <- my_df[train,]
test_data <- my_df[test,]

#######################################
#Build our First Classification Tree!##
########################################

spruce <- rpart(Purchase ~ ., 
                data = train_data,
                method = "class",
                parms = list(split = "information"),
                control = rpart.control(usesurrogate = 0, maxsurrogate = 0, minsplit = 20, minbucket = 7 ))
print(spruce)
printcp(spruce)
plot(spruce)
text(spruce,pretty = 0)
fancyRpartPlot(spruce, main = "Our Beautiful Spruce")
asRules(spruce)
summary(spruce)

####################################
#Fiddling Around with Parameters####
####################################
#Min Split and Min Bucket: changing the ending result
#Bump Min-Split
spruce.split <- rpart(Purchase ~ ., 
                data = train_data,
                method = "class",
                parms = list(split = "information"),
                control = rpart.control(usesurrogate = 0, maxsurrogate = 0, minsplit = 60))
fancyRpartPlot(spruce.split)

#Min-Bucket
spruce.bucket <- rpart(Purchase ~ ., 
                data = train_data,
                method = "class",
                parms = list(split = "information"),
                control = rpart.control(usesurrogate = 0, maxsurrogate = 0, minsplit = 25, minbucket = 50))
fancyRpartPlot(spruce.bucket)

#Shallow, introduce max depth (number of layers)
spruce.depth <- rpart(Purchase ~ ., 
                data = train_data,
                method = "class",
                parms = list(split = "information"),
                control = rpart.control(usesurrogate = 0, maxsurrogate = 0, minsplit = 25, maxdepth = 2))

fancyRpartPlot(spruce.depth)


####################################
#Build a MAXIMAL TREE###############
####################################

groot <- rpart(Purchase ~ ., 
               data = train_data,
               method = "class",
               parms = list(split = "information"),
               control = rpart.control(usesurrogate = 0, maxsurrogate = 0, minsplit = 2, minbucket = 1, cp = 0))
fancyRpartPlot(groot, main = "Maximal Groot Tree")
print(groot$cptable)
plotcp(groot)
grid()

####################################
#Prune it back######################
####################################

cverror <- groot$cptable[,'xerror']
min_cverror <- which.min(cverror)
best_cp <- groot$cptable[min_cverror, "CP"]
print(best_cp)
groot.prune <- prune(groot, cp = best_cp) #THIS IS WHAT YOU DON'T DO
printcp(groot)
groot.prune <- prune(groot, cp = .01247772)
fancyRpartPlot(groot.prune) 
print(groot.prune$cptable)
asRules(groot.prune)

################################
#Predict on the Test Set########
################################

groot.prune.predict <- predict(groot.prune, newdata=test_data, type = 'class')
table(test_data$Purchase, groot.prune.predict,dnn=c("Actual", "Predicted"))
round(100*table(test_data$Purchase, groot.prune.predict,dnn=c("% Actual", "% Predicted"))/length(groot.prune.predict))

#Build a roc plot
groot.prune.probs <- predict(groot.prune, newdata = test_data)[,2]
test_data$preds <- groot.prune.probs
roc <- roc(as.numeric(test_data$Purchase), test_data$preds)
par(mfrow = c(1,1))
plot(roc, col = 'blue', main = "ROC Curves", xlab = 'False Positive', ylab = 'True Positive')

#Compare with over-trained tree 
groot.probs <- predict(groot, newdata = test_data)
test_data$oldpreds <- groot.probs[,2]
roc2 <- roc(as.numeric(test_data$Purchase), test_data$oldpreds)
par(new = TRUE)
plot(roc2, col = 'green',xlab = 'False Positive', ylab = 'True Positive')

#Compare with default tree
spruce.probs <- predict(spruce, newdata = test_data)[,2]
roc3 <- roc(test_data$Purchase, spruce.probs)
par(new = T)
plot(roc3, col = 'red', lty = 3, lwd = 5, xlab = 'False Positive', ylab = 'True Positive')
legend("bottomright",
       legend = c("Pruned Tree","Maximal Tree","Default Tree"), 
       col = c("blue","green","red"),
       lty = c(1,1,3))

#Investigate that ending
sum(predict(spruce, newdata = test_data, type = 'class') == predict(groot.prune, newdata = test_data, type = 'class'))
length(test_data$Purchase)
par(mfrow = c(2,1))
fancyRpartPlot(groot.prune, main = 'Pruned')
fancyRpartPlot(spruce, main = 'Default')

################################
#Loss Tables and Priors#########
################################

#Building a Tree that penalizes type 1 error
loss_matrix <-  matrix(c(0,10,1,0), nrow = 2, byrow = T)
print(loss_matrix)
loss_tree <- rpart(Purchase ~ ., 
                   data = train_data,
                   method = "class",
                   parms = list(split = "information", loss = loss_matrix),
                   control = rpart.control())

fancyRpartPlot(spruce)
fancyRpartPlot(loss_tree)
#Check new predictions
loss.predict <- predict(loss_tree, newdata=test_data, type="class")
spruce.predict <- predict(spruce, newdata = test_data, type = "class")
round(100*table(test_data$Purchase, loss.predict,dnn=c("% Actual", "% Predicted"))/length(loss.predict)) #Loss Specified
round(100*table(test_data$Purchase, spruce.predict,dnn=c("% Actual", "% Predicted"))/length(spruce.predict)) #Loss NOT specified


#Specifying priors
prior_tree <- rpart(Purchase ~ ., 
                    data = train_data,
                    method = "class",
                    parms = list(split = "information", prior = c(.9,.1)),
                    control = rpart.control())
fancyRpartPlot(spruce)
fancyRpartPlot(prior_tree)
#Testing
prior.predict <- predict(prior_tree, newdata=test_data, type="class")
round(100*table(test_data$Purchase, prior.predict,dnn=c("% Actual", "% Predicted"))/length(prior.predict))
#Spruce to remind:
round(100*table(test_data$Purchase, spruce.predict,dnn=c("% Actual", "% Predicted"))/length(spruce.predict))
