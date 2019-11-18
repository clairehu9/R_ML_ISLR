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
needed <- c('ISLR', 'akima', 'splines', 'gam')   
installIfAbsentAndLoad(needed)


# Polynomial Regression and Step Functions
attach(Wage)
head(poly(age, 4, raw=T))
head(poly(age, 4, raw=F))#poly function returns matrix where each column is a linear orthogonal combination of the variables age, age^2, age^3 and age^4.
#now we fit a linear model, predicting wage using age, age^2, age^3 and age^4 as predictors
fit=lm(wage ~ poly(age, 4), data=Wage)

#we produce coefficients on each predictor: age, age^2, age^3 and age^4 
coef(summary(fit))


#here we provide an alternative way to fit the model with same predictors age, age^2, age^3 and age^4
fit2 <- lm(wage ~ poly(age, 4, raw=T), data=Wage)

#observing the coefficients we see that they are not the same as the first model, but the fit is the same
#individual coefficients in polynomial model are irrelevant/hard to interpret 
coef(summary(fit2))

#Third alternative of fitting the same function, this time we have to write out individual predictors
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)
coef(fit2a)

#Fourth alternative of fitting the same function.
fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data=Wage)

#creating grid of values for AGE at which we want predictions 
#we use generic predict (function) and specify that we want standard errors as well
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit-2*preds$se.fit)

#trying d-3 
par(mfrow=c(1,3))
fit=lm(wage ~ poly(age, 3), data=Wage)
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit-2*preds$se.fit)
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
#title("Degree-3 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)


#plotting the data and adding the degree 4 polynomial fit. 

fit=lm(wage ~ poly(age, 4), data=Wage)
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit-2*preds$se.fit)
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
#title("Degree-5 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

#verifying that the fit of the models obtrained in alternative ways is actually identical. 
#obtain value of essentially 0 
preds2 <- predict(fit2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit-preds2$fit))


#trying -10
fit=lm(wage ~ poly(age, 10), data=Wage)
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit-2*preds$se.fit)
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
#title("Degree-5 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)



#Choosing degree of polynomials by running ANOVA test on fits raning from simple MLR to d=5 polynomial 
#F-test null hypothesis: simpler model is sufficient in explaining data against a more complex model (ex. MLR vs d=2)
fit.1 <- lm(wage ~ age, data=Wage)
fit.2 <- lm(wage ~ poly(age, 2), data=Wage)
fit.3 <- lm(wage ~ poly(age, 3), data=Wage)
fit.4 <- lm(wage ~ poly(age, 4), data=Wage)
fit.5 <- lm(wage ~ poly(age, 5), data=Wage)


coef(fit.2)
coef(fit.3)
coef(fit.4)
coef(fit.5)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
#results: p-value of Model 1 to the quadratic model 2 is almost 0, meaning the fit is not sufficient. 
#Simmilar case of Model 2 to Model 3. 
# Model 3 and Model 4 perform well with 5% significance whle Model 5 is unreasonaby complex at 0.37. 

#Alternative way to get the P-values: 
coef(summary(fit.5))
(-11.983)^2 #square of the t-value are the same as the F-statistic. 

#ANOVA is useful when choosing degree of polynomial when we have other terms in the models as well: 
#don't need to use anything other than anova as we hve embabeded or nested models
#meaning that the predictors in model 1 must be a subset of the predictors in model 2. 
fit.1 <- lm(wage ~ education + age, data=Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data=Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data=Wage)
anova(fit.1, fit.2, fit.3) 

#Predicting wheather an individual earns more than $250,000 a year using the logistic regression model. 
fit <- glm(I(wage>250) ~ poly(age, 4), data=Wage, family=binomial) #True if above 250 else False: Binary
preds <- predict(fit, newdata=list(age=age.grid), se = T)
pfit <- exp(preds$fit)/(1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit-2 * preds$se.fit) 
se.bands <- exp(se.bands.logit)/(1 + exp(se.bands.logit))#Calculating the CI
#preds <- predict(fit, newdata=list(age=age.grid), type="response", se=T)


#Plotting the probabilities with CI 
#Age values that have more than 250k earning are stacked on top of the graph and those that are below are stacked at the bottom
#using jiter functin to jitter or add little bit of noise to data to avoid age values covering eachother up.  
par(mfrow=c(1,1))
plot(age, I(wage>250), xlim=agelims, type="n", ylim=c(0, .2)) 
points(jitter(age), I((wage>250)/5), cex=.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)



# Step Function 
library("ISLR")
table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data=Wage)
coef(summary(fit))
lines(table(cut(age, 4)))

par(mfrow=c(1,3))

#trying to reduce cuts to 4- we lose some data but gain new insights, big increase from 35 to 40 
reg<-lm(wage ~ cut(age, c(18, 34, 50 ,80)),data=Wage)
ypred<-predict(reg,newdata=data.frame(age=18:80),interval="c")
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
lines(18:80,ypred[,"fit"],lty=1,col="blue",lwd=2)
lines(18:80,ypred[,"lwr"],lty=2,col="red",lwd=2)
lines(18:80,ypred[,"upr"],lty=2,col="red",lwd=2)


reg<-lm(wage ~ cut(age, c(18, 25, 50, 65, 90)),data=Wage)
ypred<-predict(reg,newdata=data.frame(age=18:80),interval="c")
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
lines(18:80,ypred[,"fit"],lty=1,col="blue",lwd=2)
lines(18:80,ypred[,"lwr"],lty=2,col="red",lwd=2)
lines(18:80,ypred[,"upr"],lty=2,col="red",lwd=2)


#trying out 6 cuts - we lose patterns
reg<-lm(wage ~ cut(age, c(18, 34, 42, 51, 65, 80)),data=Wage)
ypred<-predict(reg,newdata=data.frame(age=18:80),interval="c")
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
lines(18:80,ypred[,"fit"],lty=1,col="blue",lwd=2)
lines(18:80,ypred[,"lwr"],lty=2,col="red",lwd=2)
lines(18:80,ypred[,"upr"],lty=2,col="red",lwd=2)



## Basis Function 

# Example Probelm

x <- seq(-2, 2, 0.1)
y <-  c()
for (i in x){
  if (i < 1){
    y <- c(y, 1 + i)
  }
  else{
    y <- c(y, 1 + i - 2*(i - 1)^2)
  }
}
plot(x, y, type = 'l')

# As we can esaily observe it in the 
# graphic, the equation for the first
# part of the curve forms a straight 
# line which is: y = x + 1 
# intercept : (0, 1)
# slope : 1

# and the other part of the curve
# is part of a parabola : y = -2x^2 + 5X - 1
# intercept : (0, -1)
# slope : -4x + 5(derivation)




# Practice Problem
x <- seq(-5, 5, 0.5)
y <- c()
for (i in x){
  if (i < 0){
    y <- c(y, 1)
  } else if (i >= 0 && i < 1){
    y <- c(y, 1 + i)
  } else if (i >= 1 && i <= 2){
    y <- c(y, 1 + i - (i - 1))
  } else if (i >= 2 && i <=3){
    y <- c(y, 1)
  } else if (i >=3 && i <= 4){
    y <- c(y, 1 + 3*(i - 3))
  } else if (i > 4 && i <= 5)
    y <- c(y, 1 + 3*i)
}
plot(x, y, type = 'l')


