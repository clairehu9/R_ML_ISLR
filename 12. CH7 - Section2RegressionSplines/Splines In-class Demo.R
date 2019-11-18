rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
##############################
### Load required packages ###
##############################
# 
needed <- c('ISLR', 'splines', 'boot')   
installIfAbsentAndLoad(needed)
# Polynomial Regression and Step Functions
attach(Wage)

#Polynomial Degree 3
#simply use the poly() function to create the polynomial for a single predictor
#To graph, create the sequence age.grid that has all the numbers between the minimum and maximum age
#We can then plot all predictions against age.grid
#This will be used to produce all of the graphs

fit <- lm(wage ~ poly(age, 3), data=Wage)
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Degree-3 Polynomial")
lines(age.grid, preds$fit, lwd=2, col="green")

(MSE_poly <- mean(fit$residuals^2))

#Step Function 3 knots
#Review from last class

fit.step <- lm(wage ~ cut(age, 4), data=Wage)
preds.step <- predict(fit.step, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Step Function")
lines(age.grid, preds.step$fit, lwd=2, col="blue")

(MSE_step <- mean(fit.step$residuals^2))

#Piecewise Function, break at 50
#This example is not that important, just used for an example
#Create dummy variable for if an observation is greater than the break,
#Create a variable that is the dummy variable * age (slope-dummy variable)

Break <- 50
grp <- age < Break
newdata=data.frame(age=age.grid, grpTRUE=age.grid<Break, agegrpTRUE=(age.grid< Break)*age.grid)
age_df=data.frame(wage, age, grpTRUE=age<Break, agegrpTRUE=age*(age<Break))
fit.piecewise <- lm(wage~age+grpTRUE+agegrpTRUE,data = age_df)
preds.piecewise <- predict(fit.piecewise, newdata=newdata, se=TRUE)
plot(age, wage, col="gray")
title("Piecewise Function")
lines(age.grid, preds.piecewise$fit, lwd=2, col="black")

(MSE_piece <- mean(fit.piecewise$residuals^2))


#Spline
#Use the bs() function to create the spline basis matrix
#Degree= 3 is standard
#Can either specify location of knots with knots=c() or degrees of freedom with df=int
#When df is given, creates knots at set quantiles
#The number of knots = df-1

fit1 <- lm(wage ~ bs(age, degree=3, knots=c(50)), data=Wage)
pred1 <- predict(fit1, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
title("Spline Degree 3, 1 Knot")
lines(age.grid, pred1$fit, lwd=2, col="purple")

(MSE_bs <- mean(fit1$residuals^2))

#Natural Spline
#Use the ns() function to create the natural spline basis matrix
#Degree= 3 is required; CANNOT CHANGE with this function
#Can either specify location of knots with knots=c() or degrees of freedom with df=int
#When df is given, creates knots at set quantiles
#The number of knots = df-3

fit2 <- lm(wage ~ ns(age, df=3), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
title("Natural Spline Degree 3, 1 Knot")
lines(age.grid, pred2$fit, lwd=2, col="red")

(MSE_ns <- mean(fit2$residuals^2))

#Plotting all All Together
plot(age, wage, col="gray")
lines(age.grid, preds$fit, lwd=2, col="green")
lines(age.grid, preds.step$fit, lwd=2, col="blue")
lines(age.grid, preds.piecewise$fit, lwd=2, col="black")
lines(age.grid, pred1$fit, lwd=2, col="purple")
lines(age.grid, pred2$fit, lwd=2, col="red")
legend(x="topright", legend=c("Polynomial Degree 3", "Stepwise", "Piecewise Linear", "Spline, 1 Knot", "Natural Spline Degree 3"), text.col=c("green", "blue", "black", "purple", "red"))


#Using a for loop and cv to determine which number of knots is optimal

Spline_MSE<-data.frame(matrix(nrow=0, ncol=3))
set.seed(5072)

for(i in 0:30){
  fit_temp1 <- glm(wage ~ bs(age, degree=3, df=i+3), data=Wage)
  cv1 <- cv.glm(Wage, fit_temp1, K=10)
  MSE_temp1 <- cv1$delta[1]
  fit_temp2 <- glm(wage ~ ns(age, df=i+1), data=Wage)
  cv2 <- cv.glm(Wage, fit_temp2, K=10)
  MSE_temp2 <- cv2$delta[1]
  Spline_MSE<-rbind(Spline_MSE, c(i, MSE_temp1, MSE_temp2))
}
colnames(Spline_MSE)<-c("Knots","bs_MSE", "ns_MSE")

(Spline_MSE)

plot(Spline_MSE$Knots, Spline_MSE$bs_MSE, type="b")
plot(Spline_MSE$Knots, Spline_MSE$ns_MSE, type="b")

bs(age, degree=3, df=3)
