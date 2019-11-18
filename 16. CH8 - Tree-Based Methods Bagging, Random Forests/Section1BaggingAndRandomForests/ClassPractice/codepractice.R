rm(list=ls())
#download random forest package
require(randomForest)

###Get and clean the data
churndata<-read.table("churndata.csv",sep=",",header=T)
data <- na.omit(churndata)
str(data)
data$area<-factor(data$area)




###Create training and test sets





###Grow a 500-tree forest, with mtry=4
##pay attention to the OOB error rate and confusion matrix
#rf.original<-randomforest()












###Calculate the area under the ROC curve and confidence interval 
require(pROC)




###Plot the OOB ROC curve. 
###think: what's our preference regarding the Type I/Type II trade error off for this dataset?
##hint: think from the real world business perspective 
require(verification)






###rebuild the forest by changing the vote cutoff
###Change vote cutoff to reduce Type II Error Rate (at the expense of the Type I Error Rate)
set.seed(527)
#rf.final<-randomforest()







###finally
###List the importance of the variables.




###Display a chart of Variable Importance for your final model







