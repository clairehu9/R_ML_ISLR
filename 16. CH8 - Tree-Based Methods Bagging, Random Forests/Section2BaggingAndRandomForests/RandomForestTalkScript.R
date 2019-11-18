rm(list=ls())

#####################################
###### Load required packages #######
#####################################
installIfAbsentAndLoad  <-  function(neededVector) {
        if(length(neededVector) > 0) {
                for(thispackage in neededVector) {
                        if(! require(thispackage, character.only = T)) {
                                install.packages(thispackage)}
                        require(thispackage, character.only = T)
                }
        }
}
needed <- c("randomForest", "pROC", "verification", "rpart")
#pRoc contains roc.plot
#verification contains roc.area
#rpart for building a single tree
#randomForest includes rf() for random forest model
installIfAbsentAndLoad(needed)


#####################################
#### Define evaluation functions ####
#####################################
my.auc <- function(actual, pred.prob){
        score <- roc.area(as.integer(as.factor(actual))-1, pred.prob)
        return(score)
}
my.rocPlot <- function(actual, pred.prob,auc, title){
        roc.plot(as.integer(as.factor(actual))-1, pred.prob, main="")
        legend("bottomright", bty="n",
               sprintf("Area Under the Curve (AUC) = %1.3f", auc$A))
        title(main = title,
              sub = paste("David Murray", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
}
my.typeErr <- function(table){
        print(c("Type I Error:", table[1,2] / (table[1,2] + table[1,1])))
        print(c("Type II Error:", table[2,1] / (table[2,1] + table[2,2])))
}



#####################################
###### Get and explore the data #####
#####################################
churndata <- read.table("churndata.csv",sep = ",", header = T)
data <- na.omit(churndata)
str(data)
data$area <- factor(data$area)
table(data$churn)

#####################################
### Create training and test sets ###
#####################################
nobs <- nrow(data)
set.seed(5082)
train <- sample(nobs, 0.7*nobs)
dim(data)
test <- setdiff(1:nobs, train)
train.set <- data[train,]
test.set <- data[test,]

#####################################
######### Naive Prediction ##########
#####################################
pred.naive <- rep("No", nrow(test.set))
acc.naive <- mean(test.set$churn == pred.naive)
auc.naive <- roc.area(as.integer(as.factor(test.set[, "churn"])) - 1,as.integer(as.factor(pred.naive)))
#table.naive <- table(test.set$churn,pred.naive, dnn=c("Actual", "Predicted"))

print(acc.naive)    #0.839 accuracy

#print(table.naive)

#roc.plot(as.integer(as.factor(test.set$churn))-1, as.integer(as.factor(pred.naive))-1, 
#         main = "ROC Curve for the Naive Prediction")


#####################################
######### Single Pruned Tree ########
#####################################
set.seed(5082)
rpart.fit <- rpart(churn ~ .,train.set, method = "class", parms = list(split = "information"), 
                 control = rpart.control(usesurrogate = 0, maxsurrogate = 0))
xerr <- rpart.fit$cptable[,"xerror"]
minxerr <- which.min(xerr)
mincp <- rpart.fit$cptable[minxerr,"CP"]
rpart.prune <- prune(rpart.fit,cp = mincp)

### Evaluating model####
pred.tree <- predict(rpart.prune, test.set, type = "class")
pred.tree.prob <- predict(rpart.prune, test.set, type = "prob")
acc.tree <- mean(test.set$churn == pred.tree)
table.tree<-table(test.set$churn, pred.tree ,dnn = c("Actual", "Predicted"))

print(acc.tree)    #0.896 accuracy
print(acc.naive)   

print(table.tree)
round(100* table(test.set$churn, pred.tree ,dnn = c("% Actual", "% Predicted")) / length(pred.tree))
my.typeErr(table.tree)

auc.tree <- my.auc(test.set$churn, pred.tree.prob[, "Yes"])

my.rocPlot(test.set$churn, pred.tree.prob[, "Yes"], auc.tree, title = "ROC Curve for a Single Tree")
print(auc.tree$A)  #0.786 auc

roc.plot(as.integer(as.factor(test.set$churn))-1, as.integer(as.factor(pred.naive))-1, 
         main = "ROC Curve for the Naive Prediction")
print(auc.naive$A)  #0.5 auc


#####################################
######## Bagging 500 Trees ##########
#####################################
set.seed(5082)
# set mtry = number of predictors for bagging only
bag.fit <- randomForest(formula = churn ~ ., data = train.set,
                        ntree = 500, mtry = ncol(train.set)-1,
                        importance = TRUE, localImp = TRUE, replace = TRUE)
bag.fit         #OOB 0.922 accuracy
head(bag.fit$oob.times)
head(bag.fit$err.rate)
plot(bag.fit, main = "Error Rates for Bagging Trees")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)

### Evaluating model ###
pred.bag <- predict(bag.fit, test.set, type = "class")
pred.bag.prob <- predict(bag.fit, test.set, type = "prob")
acc.bag <- mean(test.set$churn == pred.bag)
auc.bag <- my.auc(test.set$churn, pred.bag.prob[, "Yes"])
table.bag <- table(test.set$churn, pred.bag ,dnn = c("Actual", "Predicted"))

print(acc.bag)   #0.913 accuracy
print(acc.tree)
my.rocPlot(test.set$churn, pred.bag.prob[, "Yes"], auc.bag, "ROC Curve for Bagging Trees")
print(auc.bag$A) #0.837 auc
print(auc.tree$A)

print(table.bag)
round(100* table(test.set$churn, pred.bag ,dnn = c("% Actual", "% Predicted")) / length(pred.bag))
my.typeErr(table.bag)
my.typeErr(table.tree)

#####################################
###### 500 Trees Random Forest#######
#####################################
set.seed(5082)
rf.fit <- randomForest(formula = churn ~ ., data = train.set,
                        ntree = 500, mtry = 4,
                        importance = TRUE, localImp = TRUE, replace = TRUE)
rf.fit     #0.927 accuracy
bag.fit    #0.922 accuracy
head(rf.fit$err.rate)
plot(rf.fit, main = "Error Rates for Random Forest")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)


### Evaluating Model ###
pred.rf <- predict(rf.fit, test.set, type = "class")
pred.rf.prob <- predict(rf.fit, test.set, type = "prob")
acc.rf <- mean(test.set$churn == pred.rf)
auc.rf <- my.auc(test.set$churn, pred.rf.prob[, "Yes"])
table.rf <- table(test.set$churn, pred.rf ,dnn = c("Actual", "Predicted"))

print(acc.rf)   #0.913 accuracy
print(acc.bag)
my.rocPlot(test.set$churn, pred.rf.prob[, "Yes"], auc.rf, "ROC Curve for Random Forest")
print(auc.rf$A) #0.856 auc
print(auc.bag$A)

print(table.rf)
round(100* table(test.set$churn, pred.rf ,dnn = c("% Actual", "% Predicted")) / length(pred.rf))
my.typeErr(table.rf)
my.typeErr(table.bag)

#####################################
####### Cross validate mtry #########
#####################################
oob.err<-c()
test.err<-c()
set.seed(5082)
for(i in 1:16){
  cv.fit <- randomForest(formula = churn ~ ., data = train.set, ntree=500, mtry = i,
                      importance=TRUE,localImp=TRUE,replace=TRUE)
  oob.err[i] = cv.fit$err.rate[500]
  pred.cv <- predict(cv.fit, test.set)
  test.err[i] <- mean(test.set$churn  != pred.cv)
  cat(i," ")
}
matplot(1:16, cbind(test.err,oob.err), pch=19, col=c("red","blue"), type="b", 
        xlab = "# of mtry", ylab = "Error Rate")
title(main = "Error Rates for Random Forest of different mtry")
legend("topright",legend=c("Test error ","OOB error"),pch=19,col=c("red","blue"))
best.mtry <- which(oob.err == min(oob.err))

# Rebuild the model with number of trees that minimizes the OOB error rate
# use the first one if there are more than one
plot(rf.fit, main = "Error Rates for Random Forest")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)

min.oob.err <- min(rf.fit$err.rate[,"OOB"])
min.index <- which(rf.fit$err.rate[,"OOB"] == min.oob.err)
min.index # if have mutiple ones, choose the first one

set.seed(5082)
rf2.fit <- randomForest(formula = churn ~ ., data = train.set, ntree = min.index, mtry = 4,
                   importance = TRUE, localImp = TRUE, replace = TRUE)

pred.rf2 <- predict(rf2.fit, test.set, type = "class")
pred.rf2.prob <- predict(rf2.fit, test.set, type = "prob")
acc.rf2 <- mean(test.set$churn == pred.rf2)
auc.rf2 <- my.auc(test.set$churn, pred.rf2.prob[, "Yes"])
table.rf2 <- table(test.set$churn, pred.rf2 ,dnn = c("Actual", "Predicted"))

print(acc.rf2)   #0.912 accuracy
print(acc.rf)    #0.913 accuracy
print(acc.bag)   #0.913 accuracy
my.rocPlot(test.set$churn, pred.rf2.prob[, "Yes"], auc.rf2, "ROC Curve for Random Forest")
print(auc.rf2$A) #0.8563 auc
print(auc.rf$A)  #0.8558 auc
print(auc.bag$A) #0.8374 auc

print(table.rf2)
round(100* table(test.set$churn, pred.rf2 ,dnn = c("% Actual", "% Predicted")) / length(pred.rf2))
my.typeErr(table.rf2)
my.typeErr(table.rf)
my.typeErr((table.bag))
#####################################
####### variable Importance #########
#####################################

importance(rf2.fit)[order(importance(rf2.fit)[,"MeanDecreaseAccuracy"], decreasing=T),]
varImpPlot(rf2.fit, main="Variable Importance in the Random Forest")


### How about removing negative MeanDecreaseAccuracy variables? ###
rm <- c("night.calls", "area","day.calls","eve.calls")
train.left <- train.set[, !names(train.set) %in% rm] 
test.left <- test.set[, !names(test.set) %in% rm]
rf.fit.left <- randomForest(formula = churn ~ .,data = train.left,ntree = min.index, mtry=4,
                     importance = TRUE, localImp = TRUE, replace = TRUE)

### Evaluating Model ###
pred.rf.left <- predict(rf.fit.left, test.set, type = "class")
pred.rf.left.prob <- predict(rf.fit.left, test.set, type = "prob")
acc.rf.left <- mean(test.set$churn == pred.rf.left)
auc.rf.left <- my.auc(test.set$churn, pred.rf.left.prob[, "Yes"])
table.rf.left <- table(test.set$churn, pred.rf.left ,dnn = c("Actual", "Predicted"))

print(acc.rf.left)   #0.918 accuracy
print(acc.rf2)       #0.912 accuracy
my.rocPlot(test.set$churn, pred.rf.left.prob[, "Yes"], auc.rf.left, "ROC Curve for Random Forest")
print(auc.rf.left$A) #0.860 auc
print(auc.rf2$A)     #0.856 auc

print(table.rf.left)
round(100* table(test.set$churn, pred.rf.left ,dnn = c("% Actual", "% Predicted")) / length(pred.rf.left))
my.typeErr(table.rf.left)
my.typeErr(table.rf2)

#####################################
########### Change Cutoff############
#####################################
rf.fit.cut <- randomForest(formula = churn ~ ., data = train.left, ntree = min.index, mtry = 4,
                     importance = TRUE, localImp = TRUE, na.action = na.roughfix, 
                     replace = TRUE, cutoff = c(0.8,0.2))
pred.rf.cut <- predict(rf.fit.cut, test.set, type = "class")
table.rf.cut <- table(test.set$churn, pred.rf.cut, dnn = c("Actual", "Predicted"))
print(table.rf.cut)
round(100* table(test.set$churn, pred.rf.cut ,dnn = c("% Actual", "% Predicted")) / length(pred.rf.cut))
my.typeErr(table.rf.cut)
my.typeErr(table.rf.left)

#####################################
#### Compare with other models ######
#####################################
set.seed(5082)
# Logistic Regression
logreg <- glm(formula = churn ~ .,data = train.set, family = binomial)
prtest_logreg_prob <- predict(logreg, test.set,type = "response")
prtest_logreg <- ifelse(prtest_logreg_prob > 0.5,"Yes","No")
#aucc_test_logreg <- roc.area(as.integer(as.factor(test.set[, "churn"]))-1,as.integer(as.factor(prtest_logreg)))
mean(test.set$churn==prtest_logreg) #0.853

# LDA
lda.fit<-lda(churn ~ .,data=train.set)
lda.pred<-predict(lda.fit, test.set)
mean(test.set$churn==lda.pred$class) #0.851


# QDA
qda.fit  <- qda(churn ~ .,data=train.set)
qda.pred <- predict(qda.fit, test.set)
mean(test.set$churn==qda.pred$class) #0.872

# KNN
require(class)
accs <- c()
data$vmail <- as.numeric(data$vmail)
train.X <- data[train,-1]
test.X <- data[-train,-1]
train.y <- data[train,1]
test.y <- data[-train,1]
kset <- seq(1,30,2)    
for(i in 1:length(kset)){
  k <- kset[i]
  knn.pred <- knn(train.X, test.X,train.y, k=k)
  acc <- mean(test.y==knn.pred)
  accs[i] <- acc
}
k_best <- kset[which.max(accs)]
knn.pred <-knn(train.X, test.X,train.y, k=k_best)

acc_best <- max(accs)
acc_best      #0.873


   ### final ensemble#####
l_r <- ifelse(prtest_logreg=="No",1,2)
k_r <- as.numeric(knn.pred)
lda_r <- as.numeric(lda.pred$class)
qda_r <- as.numeric(qda.pred$class)
r_r <- as.numeric(pred.rf.left)
all_results <- cbind(l_r,k_r,lda_r,qda_r,r_r)
all_n <- apply(all_results,1,sum)
final <- ifelse(all_n<8, "No","Yes")
mean(final==test.set$churn)
