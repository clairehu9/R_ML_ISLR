rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c("pls", "MASS")  
installIfAbsentAndLoad(needed)

##################################################
# Principal Components Regression - Three ways
##################################################
data <- data.frame(scale(na.omit(Boston)))
n <- nrow(data)
set.seed(5072)
train <- sample(n, n * 0.80)
test <- setdiff(1:n, train)
## PCR Method 1 - Use the built-in function pcr() ##

pcr.fit <- pcr(crim ~ ., 
               data=data, 
               subset=train,
               scale=F, 
               validation="CV")
validationplot(pcr.fit, val.type="MSEP")

# M = 3 yields a sufficiently low cross-validation MSE. We
# compute the test MSE as follows.
n.pcs <- 3   
pcr.pred.train <- predict(pcr.fit, 
                          data[train,], 
                          ncomp=n.pcs, 
                          type='response')
# Train error rate of PCR
mean((pcr.pred.train - data[train,1])^2)

pcr.pred <- predict(pcr.fit, 
                    data[test,], 
                    ncomp=n.pcs, 
                    type='response')
mean((pcr.pred - data[test,1])^2)
# Test error rate of PCR

## PCR Method 2 - Perform PCA and then regression with glm ##

#Perform PCA
pca <- prcomp(data[train, -1], scale=F, center = F)
summary(pca)

# Perform the linear combinations on both the training and
# test sets using the specified number of principal
# components to create the Z matrix...
y.train <- data$crim[train]
Z.train <- data.frame(data.frame(as.matrix(data[train,-1]) %*% 
                                   as.matrix(pca$rotation[,1:n.pcs])))
names(Z.train) <- paste('Z', 1:n.pcs, sep='')
head(Z.train)

y.test <- data$crim[test]
Z.test <- data.frame(data.frame(as.matrix(data[test,-1]) %*% 
                                  as.matrix(pca$rotation[,1:n.pcs])))
names(Z.test) <- paste('Z', 1:n.pcs, sep='')
head(Z.test)

# Fit a glm model
glm.fit <- glm(y.train ~ ., data=Z.train)
# Train error
glm.pred.train <- predict(glm.fit, type='response')
mean((glm.pred.train - y.train) ^ 2)
# Test error
glm.pred <- predict(glm.fit, newdata = Z.test, type='response')
mean(as.vector((glm.pred - y.test) ^ 2)[1])

## PCR Method 3 - Perform PCA and then regression with lm ##

# Fit an lm model just to illustrate that result is the same...
lm.fit <- lm(y.train ~ ., data=Z.train)
# No newdata parameter in the following means use the train data
lm.pred.train <- predict(lm.fit, type='response')  
# Train error
mean((lm.pred.train - y.train) ^ 2)
# Test error
lm.pred <- predict(lm.fit, newdata = Z.test, type='response')
mean((lm.pred - y.test) ^ 2)

# Another way to get the prediction...
col.names <- paste('PC', 1:n.pcs, sep='')
prin.comps <- matrix(pca$rotation[, 1:n.pcs], 
                     ncol=n.pcs, 
                     dimnames=list(names(data[,-1]), col.names))
head(prin.comps)
# Create the linear combinations using the required number of principal components
testdata <- data.frame(data[test, 1], 
                       as.matrix(data[test,-1]) %*% prin.comps)
names(testdata) <- c('crim', paste('Z', 1:n.pcs, sep=''))
head(testdata)
# Create a model matrix
MM <- model.matrix(crim ~ ., data = testdata)
# Multiply the model matrix by the coefficients produced by glm
lm.pred.alt <- MM %*% lm.fit$coefficients
# test error rate (same as above using predict())
mean((lm.pred.alt - y.test) ^ 2)
##################################################
# Just for comparison, Partial Least-Squares Regression
##################################################
pls.fit <- plsr(crim ~ ., data=Boston, 
                subset=train, 
                scale=F, 
                validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

pls.pred <- predict(pls.fit, 
                    data[test,], 
                    ncomp=3, 
                    type='response')
mean((pls.pred - y.test)^2)

#########################################################
# Summary - first few rows of each prediction.
# Observe that the pcr, glm and lm models are very close.
#########################################################
results <- data.frame(pcr.pred, 
                      glm.pred, 
                      lm.pred.alt, 
                      pls.pred)
names(results) <- c('pcr', 'glm', 'lm', 'pls')
head(results)

