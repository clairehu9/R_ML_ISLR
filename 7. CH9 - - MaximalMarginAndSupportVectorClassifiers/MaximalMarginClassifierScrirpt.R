rm(list=ls())
#########################################################
### Functions
#########################################################
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
needed <- c('e1071')      
installIfAbsentAndLoad(needed)     
# Build toy data that is perfectly separable
set.seed(5082)
x <- matrix(rnorm(20 * 2), ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1,] <- x[y == 1,] + 2.5
plot(x[, 2], x[, 1], col=(3 - y))
# Create a dataframe with the response 
# variable converted to a factor to
# indicate classification
dat <- data.frame(x=x, y=as.factor(y))
# Use the svm() function with a very large 
# cost (so that the margin is essentially 
# impenetrable)
svmfit <- svm(y~., 
              data=dat, 
              kernel="linear", 
              cost=1e5)
summary(svmfit)
plot(svmfit, dat)

