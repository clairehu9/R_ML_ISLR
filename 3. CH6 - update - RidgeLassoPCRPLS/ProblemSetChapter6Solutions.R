rm(list=ls())
###################################################
### Functions
###################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
###################################################
### Load required packages ###
###################################################
# the pls package contains the pls() and plse() functions
needed <- c("glmnet", "pls", "ISLR", "MASS")  
installIfAbsentAndLoad(needed)
##########################################
#### PROBLEM 1a                      ####
##########################################
n <- dim(College)[1]
p <- dim(College)[2]
set.seed(5082)
# Scale the numeric variables
College[3:18] <- data.frame(scale(College[-c(1, 2)]))
train = sample(n, .8 * n)
test = -train
College.train = College[train, ]
College.test = College[test, ]
##########################################
#### PROBLEM 1b                      ####
##########################################
glm.fit = glm(Apps~., data=College.train)
glm.pred = predict(glm.fit, newdata=College.test)
(glm.test.MSE <- mean((College.test$Apps - glm.pred)^2))
##########################################
#### PROBLEM 1c                      ####
##########################################
(glm.coefficients <- coef(glm(Apps~., data=College)))
##########################################
#### PROBLEM 1d                      ####
##########################################
grid = 10 ^ seq(10, -4, length=100)
##########################################
#### PROBLEM 1e                      ####
##########################################
train.mat = model.matrix(Apps~., data=College.train)[,-1]
test.mat = model.matrix(Apps~., data=College.test)[,-1]
set.seed(5082)
mod.ridge = cv.glmnet(train.mat, 
                      College.train[, "Apps"],
                      alpha=0, 
                      lambda=grid)
##########################################
#### PROBLEM 1f                      ####
##########################################
plot(mod.ridge)
##########################################
#### PROBLEM 1g                      ####
##########################################
(lambda.best = mod.ridge$lambda.min)
##########################################
#### PROBLEM 1h                      ####
##########################################
ridge.pred = predict(mod.ridge, 
                     newx=test.mat, 
                     s=lambda.best)
(ridge.test.MSE <- mean((College.test$Apps - ridge.pred)^2))
##########################################
#### PROBLEM 1i                      ####
##########################################
# Construct a finalridge model using the full dataset
ridge.mod.final <- glmnet(model.matrix(Apps~., College)[, -1], 
                          College$Apps, 
                          alpha=0, 
                          lambda=grid)
# The Ridge coefficients:
(ridge.coefficients <- predict(ridge.mod.final, 
        s=lambda.best, 
        type="coefficients"))
##########################################
#### PROBLEM 1j                      ####
##########################################
data.frame("glm"=glm.coefficients, "ridge"=as.vector(ridge.coefficients))
##########################################
#### PROBLEM 1k.e                    ####
##########################################
set.seed(5082)
mod.lasso = cv.glmnet(train.mat, 
                      College.train[, "Apps"],
                      alpha=1, 
                      lambda=grid)
##########################################
#### PROBLEM 1k.f                    ####
##########################################
plot(mod.lasso)
##########################################
#### PROBLEM 1k.g                    ####
##########################################
(lambda.best = mod.lasso$lambda.min)
##########################################
#### PROBLEM 1k.h                    ####
##########################################
lasso.pred = predict(mod.lasso, 
                     newx=test.mat, 
                     s=lambda.best)
(lasso.test.MSE <- mean((College.test$Apps - lasso.pred)^2))
##########################################
#### PROBLEM 1k.i                    ####
##########################################
# Construct a finallasso model using the full dataset
lasso.mod.final <- glmnet(model.matrix(Apps~., College)[, -1], 
                          College$Apps, 
                          alpha=1, 
                          lambda=grid)
# The lasso coefficients:
(lasso.coefficients <- predict(lasso.mod.final, 
                               s=lambda.best, 
                               type="coefficients"))
data.frame("glm"=glm.coefficients, 
           "ridge"=as.vector(ridge.coefficients), 
           "lasso"=as.vector(lasso.coefficients))
##########################################
#### PROBLEM 1l                       ####
##########################################
# The very minor shrinkage occurring here suggests that
# whatever variance reduction is occurring due to the
# shringage of the coefficients is not sufficiently large to
# overcome the corresponding increase in the squared bias.
##########################################
#### PROBLEM 1m                       ####
##########################################
set.seed(5082)
pcr.fit <- pcr(Apps~., 
               data=College, 
               subset=train, 
               scale=TRUE, 
               validation="CV")
##########################################
#### PROBLEM 1n                       ####
##########################################
validationplot(pcr.fit, val.type="MSEP")
##########################################
#### PROBLEM 1o                       ####
##########################################
pcr.pred <- predict(pcr.fit, newdata=College.test, ncomp=8)
(pcr.test.MSE <- mean((pcr.pred-College.test$Apps)^2))
# The MSEP obtained with 8 coefficients was significantly
# greater than the MSEP obtained in part b which used the
# full model, indicating that this was not a good idea.
#
##########################################
#### PROBLEM 1p.m                     ####
##########################################
set.seed(5082)
pls.fit <- plsr(Apps~., 
               data=College, 
               subset=train, 
               scale=TRUE, 
               validation="CV")
##########################################
#### PROBLEM 1p.n                     ####
##########################################
validationplot(pls.fit, val.type="MSEP")
##########################################
#### PROBLEM 1p.o                     ####
##########################################
pls.pred <- predict(pls.fit, newdata=College.test, ncomp=9)
(pls.test.MSE <- mean((pls.pred-College.test$Apps)^2))
# The MSEP obtained with 9 coefficients was slightly lower
# than the MSEP obtained in part b which used the full
# model, indicating that this was a good idea. The simpler
# model has the potential to have lower variance that the
# full model without sacrificing predictive power.
# 
##########################################
#### PROBLEM 1q                       ####
##########################################
data.frame("glm"=glm.test.MSE, 
           "ridge"=ridge.test.MSE, 
           "lasso"=lasso.test.MSE,
           "PCR"=pcr.test.MSE, 
           "PLS"=pls.test.MSE)
(y.bar <- mean(College$Apps))
(upper <- y.bar+1.96*sqrt(pls.test.MSE))
(lower <- y.bar-1.96*sqrt(pls.test.MSE))

# A rough-cut confidence interval around a mean prediction 
# of 3001 apps is about [421, 5582], which is a VERY wide 
# interval. Consequently, I would not have a great deal of 
# confidence in a prediction from any of these models.
# 
# For a more precise calculation of a confidence interval 
# (and a preduction interval), search for those topics in
# the Semester 1 talk on multiple Linear Regression.
##########################################
#### PROBLEM 2a                       ####
##########################################
# 
# Lasso
x = model.matrix(crim ~ ., data = Boston)[, -1]
y = Boston$crim
cv.lasso = cv.glmnet(x, y, type.measure = "mse")
plot(cv.lasso)
# lambda.1se element of the cv.glm object for lasso and 
# ridge is largest value of lambda such that error is within
# 1 standard error of the minimum. The largest value of lambda
# produces the most-shrunken (simplest) model.
cv.lasso$lambda.1se
lambda.lasso <- which(cv.lasso$lambda == cv.lasso$lambda.1se)
# The cvm member provides the mean errors associated with
# each vaue of lambda
MSE.lasso <- cv.lasso$cvm[lambda.lasso]
# 
# Ridge
cv.ridge = cv.glmnet(x, y, type.measure = "mse", alpha = 0)
plot(cv.ridge)
cv.ridge$lambda.1se
lambda.ridge <- which(cv.ridge$lambda == cv.ridge$lambda.1se)
MSE.ridge <- cv.ridge$cvm[lambda.ridge]
# PCR
pcr.fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.MSEs <- pcr.fit$validation$adj[1,]
pcr.min.MSE <- pcr.MSEs[which.min(pcr.MSEs)]
pcr.MSE.sd <- sd(pcr.MSEs[3:13])
pcr.1sd.candidate.indices <- which(pcr.MSEs < pcr.min.MSE + pcr.MSE.sd)
pcr.numcomponents.simplest <- pcr.1sd.candidate.indices[which.min(pcr.1sd.candidate.indices)]
MSE.pcr <- pcr.MSEs[pcr.numcomponents.simplest]
# 
# PLSR
plsr.fit = plsr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
validationplot(plsr.fit, val.type="MSEP")
plsr.MSEs <- plsr.fit$validation$adj[1,]
plsr.min.MSE <- plsr.MSEs[which.min(plsr.MSEs)]
plsr.MSE.sd <- sd(plsr.MSEs[2:13])
plsr.1sd.candidate.indices <- which(plsr.MSEs < plsr.min.MSE + plsr.MSE.sd)
plsr.numcomponents.simplest <- plsr.1sd.candidate.indices[which.min(plsr.1sd.candidate.indices)]
MSE.plsr <- plsr.MSEs[plsr.numcomponents.simplest]
##########################################
#### PROBLEM 2b                       ####
##########################################
data.frame(MSE.lasso, MSE.ridge, MSE.pcr, MSE.plsr)
#
# The principal component and partial least squares 
# regressions outperformed the ridge and lasso models on this
# dataset. I would choose the partial least squares model
# using 4 partial least squares directions. 
########################################## 
### PROBLEM 2c                       #####
##########################################
#
# Yes, the plsr model involves all the features of the
# original dataset. Each partial least squares direction is
# computed using information from all 13 predictors.
