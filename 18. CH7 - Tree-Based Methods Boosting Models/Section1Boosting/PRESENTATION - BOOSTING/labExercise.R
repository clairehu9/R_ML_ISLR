rm(list=ls())
#require(rattle)

#ada function => boost function (AdaBoost) 
require(ada)
require(ISLR) 
attach(Hitters)

#omit rows with NA 
myData <- ????(Hitters)
head(myData) 

#calculate the average salary
averageSalary <- ???(myData???) / ???(myData)

#turn salary into a categorical variable (above and below the average) 
for(i in 1:nrow(myData)) { 
  if(myData$Salary[i] >= averageSalary) { 
    myData$Salary[i] <- "Yes"   
  }  else { 
    myData$Salary[i] <- "No"  
  }
}
head(myData) 

#set variable 'nobs' to number of rows in dataset 
nobs <- ???(myData)

#set your seed to 595
???(595)

#separate data into 80/20 split 
train <- sample(nobs, ???*nobs)
test <- ???(1:nobs,train)

#predict salary againgst all other varaibles
#use only the train data 
#originally use 50 iterations 
#use a 0.5 bag fraction 
boostingModel_1 <- ada(formula=???,data=???,iter=???,bag.frac=???,control=rpart.control(maxdepth=30,cp=0.01,minsplit=20,xval=10))
print(boostingModel_1)  

# Evaluate by scoring the test set
#predict the model using the testing data 
prtest <- ???(boostingModel_1, newdata=myData[???])
myTable <- table(myData[test,"Salary"], prtest,dnn=c("Actual", "Predicted"))
myTable

#calculate the error rate from the confusion matrix 
errorRate_1 <- (myTable[???] + myTable[???])/???(myData)
errorRate_1


#congrats!  You completed the template! 
#Now, edit your model to see if you can get a lower error Rate 

