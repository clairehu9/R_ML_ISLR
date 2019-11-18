rm(list=ls())

library(splines)
library(boot)
nba <- read.csv("nba_value.csv", as.is = T)
attach(nba)

###### Exercise 1 #######

# 1. Use bs() to fit a regression spline to predict VALUE using AVG. 
#    Hint:(1) Use cubic spline to fit each region
#         (2) Use only 1 knot here and choose the location on your own
fit.nba <- lm(VALUE_MILLIONS ~ bs(AVG, degree=3, knots=c(18000)), data=nba)
nbalims <- range(AVG)                            
nba.grid <- seq(from=nbalims[1], to=nbalims[2])  
pred.nba <- predict(fit.nba, newdata=list(AVG=nba.grid), se=T)

# 2. Calculate the MSE
(MSE_bs <- mean(fit.nba$residuals^2))


# 3. Plot the resulting fit and also the vertical line of where the knot is
#    Hint: make sure to give the plot a title
plot(AVG, VALUE_MILLIONS, col="black")
abline(v=18000, col='red', lwd=3)
title("Spline Degree 3, 1 Knot")
lines(nba.grid, pred.nba$fit, lwd=2, col="purple")

###### Exercise 2 #######

# 1. Use bs() to fit a regression spline again and choose the number of knots and also the locations
#    Hint: use cubic spline to fit each region
fit.nba1 <- lm(VALUE_MILLIONS ~ bs(AVG, degree=3, knots=c(15500,16500,19000,20000)), data=nba)
pred.nba1 <- predict(fit.nba1, newdata=list(AVG=nba.grid), se=T)

# 2. How did you choose the # of knots and where were the knots placed in the previous question? 
print('We look through the distribution of the data and put knots at those places which have more data.')

# 3. Calculate the MSE
(MSE_bs1 <- mean(fit.nba1$residuals^2))

# 4. Plot the resulting fit and also the vertical lines of where these knots are
plot(AVG, VALUE_MILLIONS, col="black")
abline(v=c(15500,16500,19000,20000), col='red', lwd=3)
title("Spline Degree 3, 4 Knot")
lines(nba.grid, pred.nba1$fit, lwd=2, col="blue")

fit.nba2 <- lm(VALUE_MILLIONS ~ bs(AVG, degree=3, df=7))
pred.nba2 <- predict(fit.nba1, newdata=list(AVG=nba.grid), se=T)
(MSE_bs2 <- mean(fit.nba2$residuals^2))
attr(bs(AVG,df=7),"knots")
###### Exercise 3 #######

# 1. Use bs() and ns() to fit a regression spline for a range of degrees of freedom 
#    Hint:you will decide the range of df on your own 
Spline_MSE<-data.frame(matrix(nrow=0, ncol=3))

for(i in 4:30){
  fit_temp1 <- lm(VALUE_MILLIONS ~ bs(AVG, degree=3, df=i), data=nba)
  MSE_temp1 <- mean(fit_temp1$residuals^2)
  fit_temp2 <- lm(VALUE_MILLIONS ~ ns(AVG, df=i), data=nba)
  MSE_temp2 <- mean(fit_temp2$residuals^2)
  Spline_MSE<-rbind(Spline_MSE, c(i, MSE_temp1, MSE_temp2))
}
# 2. Plot the resulting fits
colnames(Spline_MSE)<-c("DF","bs_MSE", "ns_MSE")
plot(Spline_MSE$DF, Spline_MSE$bs_MSE)
plot(Spline_MSE$DF, Spline_MSE$ns_MSE)
# 3. Report the associated MSE in a table
Spline_MSE
# 4. Set seed to 5072
set.seed(5072)
# 5. Do cross-validation to decide the optimal number of knots 
Spline_MSE_cv<-data.frame(matrix(nrow=0, ncol=3))

for(i in 0:30){
  fit_temp3 <- glm(VALUE_MILLIONS ~ bs(AVG, degree=3, df=i+3), data=nba)
  cv1 <- cv.glm(nba, fit_temp3, K=10)
  MSE_temp3 <- cv1$delta[1]
  fit_temp4 <- glm(VALUE_MILLIONS ~ ns(AVG, df=i+1), data=nba)
  cv2 <- cv.glm(nba, fit_temp4, K=10)
  MSE_temp4 <- cv2$delta[1]
  Spline_MSE_cv<-rbind(Spline_MSE_cv, c(i, MSE_temp3, MSE_temp4))
}


# Boundary.knots	
# boundary points at which to anchor the B-spline basis (default the range of the non-NA data). If both knots and Boundary.knots are supplied, the basis parameters do not depend on x. Data can extend beyond Boundary.knots.

# 6. Report the associated knots and MSEs in a table
colnames(Spline_MSE_cv)<-c("Knots","bs_MSE", "ns_MSE")
Spline_MSE_cv
# 7. Plot the results 
plot(Spline_MSE_cv$Knots, Spline_MSE_cv$bs_MSE)
plot(Spline_MSE_cv$Knots, Spline_MSE_cv$ns_MSE)

