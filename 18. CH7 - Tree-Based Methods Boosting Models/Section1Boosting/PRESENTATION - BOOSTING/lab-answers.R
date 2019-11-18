rm(list=ls())
#require(rattle)

#ada function => boost function (AdaBoost) 
require(ada)
require(ISLR) 
attach(Hitters)

#omit rows with NA 
myData <- na.omit(Hitters)
head(myData)

#turn salary into a categorical variable (above and below the average) 
averageSalary <- sum(myData$Salary) / nrow(myData)

for(i in 1:nrow(myData)) { 
  if(myData$Salary[i] >= averageSalary) { 
    myData$Salary[i] <- "Yes"   
  }  else { 
    myData$Salary[i] <- "No"  
  }
}
head(myData) 

#set variable to number of rows in dataset 
nobs <- nrow(myData)
set.seed(595)

#separate into 80/20 split 
train <- sample(nobs, 0.8*nobs)
test <- setdiff(1:nobs,train)

#x is descriptions and y is responses 
#iter = 50 is the number of times the bagging/model creation step occurs (50 is default) 
boostingModel_1 <- ada(formula=Salary~.,data=myData[train,],iter=50,bag.frac=0.5,control=rpart.control(maxdepth=30,cp=0.01,minsplit=20,xval=10))
print(boostingModel_1)  

# Evaluate by scoring the test set
prtest <- predict(boostingModel_1, newdata=myData[test,])
myTable <- table(myData[test,"Salary"], prtest,dnn=c("Actual", "Predicted"))
myTable

#calculate the error rate 
errorRate_1 <- (myTable["No","Yes"] + myTable["Yes","No"])/nrow(myData)
errorRate_1


#edit your model to see if you can get a lower error Rate 



