######## Problem 1############

# In this problem, you will input the mall dataset to fit regression splines.
# Recall that we are still in a one-predictor case here. 

# Part 1: Load packages and dataset

# a) Install required packages: splines
library(splines)
# b) Load the dataset Position Salary into R (file: Mall_Customers.csv)
mall <- read.csv("C:/Users/user/Desktop/W&M BA Fall/Course/Spring Semester/Machine Learning II/HW/Seminar/exercise/Mall_Customers.csv", as.is = T)
# c) Use attach() to make the database be attached to the R search path.
attach(mall)
# d) Use summary() and str() to take a brief observation of the dataset
str(mall)
summary(mall)

# Part 2: Fit a regression spline to predict spending score using age

# e) Use bs() to fit regression splines to predict spending score using age
#    Hint: (1) Use cubic spline to fit each region
#          (2) Use only 1 knot here and choose the location on your own
fit1 <- lm(Spending.Score..1.100. ~ bs(Age, degree=3, knots=c(45)), data=mall)
age.lims <- range(Age)                            
age.grid <- seq(from=age.lims[1], to=age.lims[2])  
pred1 <- predict(fit1, newdata=list(Age=age.grid), se=T)
# f) Explain how you decide the location of the knot
print ('We put the knot at 45, which is the median of age in this dataset')
# g) Calculate the MSE of the regression splines
MSE_bs <- mean(fit1$residuals^2)
MSE_bs
# h) Plot the resulting fit and also the vertical line of where the knot is 
plot(Age, Spending.Score..1.100., col="black")
title("Spline Degree 3, 1 Knot")
lines(age.grid, pred1$fit, lwd=2, col="purple")
abline(v=45, col='red', lwd=3)

# Part 3: Fit a regression spline again to predict spending score using age

# i) Use bs() to fit regression splines again and choose the number of knots and also the locations on your own
fit2 <- lm(Spending.Score..1.100. ~ bs(Age, degree=3, knots=c(25,40,60)), data=mall)
age.lims <- range(Age)                            
age.grid <- seq(from=age.lims[1], to=age.lims[2])  
pred2 <- predict(fit2, newdata=list(Age=age.grid), se=T)
# j) Explain how you decide the number and locations of these knots
print('We look through the distribution of the data and put knots at those places which have more data.')
# k) Plot the resulting fit and also the vertical lines of where these knots are for exercise i
plot(Age, Spending.Score..1.100., col="black")
title("Spline Degree 3, 3 Knots")
lines(age.grid, pred2$fit, lwd=2, col="blue")
abline(v=c(25,40,60), col='red', lwd=3)

# l) Use bs() to fit regression splines again 
#    and use attr(bs(…),’knots’) to return the position of the knots 
#    Hint: (1) this time, let R place the corresponding number of knots at uniform quantiles of the data automatically
#          (2) make sure the number of knots are the same as exercise i
fit3 <- lm(Spending.Score..1.100. ~ bs(Age, degree=3, df=6))
attr(bs(Age,df=6),"knots") 
# m) Calculate the MSE of the regression splines for both the manual-decided one and the R-decided one and compare the MSEs
MSE_bs2 <- mean(fit2$residuals^2)
MSE_bs3 <- mean(fit3$residuals^2)
MSE_bs2
MSE_bs3


######## Problem 2 ###########
# In this problem, you will input the mall dataset to fit regression splines and natural splines.

# Part 1: Fit a regression spline and a natural spline to predict spending score using age

# a) Use bs() and ns() to fit a regression spline for a range of degrees of freedom from 3 to 30
Spline_MSE<-data.frame(matrix(nrow=0, ncol=3))

for(i in 3:30){
  fit_temp1 <- lm(Spending.Score..1.100. ~ bs(Age, degree=3, df=i), data=mall)
  MSE_temp1 <- mean(fit_temp1$residuals^2)
  fit_temp2 <- lm(Spending.Score..1.100. ~ ns(Age, df=i), data=mall)
  MSE_temp2 <- mean(fit_temp2$residuals^2)
  Spline_MSE<-rbind(Spline_MSE, c(i, MSE_temp1, MSE_temp2))
}

# c) Report the associated MSE in a table
colnames(Spline_MSE)<-c("DF","bs_MSE", "ns_MSE")
Spline_MSE

# d) Plot the results 
plot(Spline_MSE$DF, Spline_MSE$bs_MSE)
plot(Spline_MSE$DF, Spline_MSE$ns_MSE)

# Part 2: Do cross-validation to determine the optimal number of knots

# e) Set seed to 5072
set.seed(5072)
# f) Use cv.glm() to do cross validation here to decide how many knots to locate in order to generate the lowest MSE
Spline_MSE_cv<-data.frame(matrix(nrow=0, ncol=3))

for(i in 0:30){
  fit_temp3 <- glm(Spending.Score..1.100. ~ bs(Age, degree=3, df=i+3), data=mall)
  cv1 <- cv.glm(mall, fit_temp3, K=10)
  MSE_temp3 <- cv1$delta[1]
  fit_temp4 <- glm(Spending.Score..1.100. ~ ns(Age, df=i+1), data=mall)
  cv2 <- cv.glm(mall, fit_temp4, K=10)
  MSE_temp4 <- cv2$delta[1]
  Spline_MSE_cv<-rbind(Spline_MSE_cv, c(i, MSE_temp3, MSE_temp4))
}
# g) Report the associated knots and MSEs in a table
colnames(Spline_MSE_cv)<-c("Knots","bs_MSE", "ns_MSE")
Spline_MSE_cv
# h) Plot the results 
plot(Spline_MSE_cv$Knots, Spline_MSE_cv$bs_MSE)
plot(Spline_MSE_cv$Knots, Spline_MSE_cv$ns_MSE)

