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
needed <- c('caret', 'doParallel', 'snow', 'e1071', 'kernlab')      
installIfAbsentAndLoad(needed)
set.seed(5082)

# Generate a data set with n = 500 and p = 2, such that the observations
# belong to two classes with a quadratic decision boundary
# between them.
n = 1000
p = 2 
# x1 = runif(n) - 0.5
# x2 = runif(n) - 0.5
x1 = rnorm(n) - 0.5
x2 = rnorm(n) - 0.5
y <- factor(ifelse(abs(x1) - abs(x2) < 0, 'Red', 'Black'), order=T, levels <- c('Red', 'Black'))
colorsForPlot <- ifelse(y == 'Red', 1, 2)
# y = 1 * ( abs(x1) - abs(x2) > 0)   
my.df = data.frame(x1=x1, x2=x2, y)
# Plot the observations, colored according to their class labels.
plot(x1, x2, col=colorsForPlot, pch=19, cex=1.05, xlab='x1', ylab='x2', main='initial data')

sigma.dist <- sigest(y ~ ., data = my.df, frac = 1)
svmTuneGrid <- data.frame(.sigma = c(rep(sigma.dist[1], 10), 
                                     rep(sigma.dist[2], 10), 
                                     rep(sigma.dist[3], 10)), 
                          .C = 2^(-2:7), row.names=NULL)
svmTuneGrid
############################################## 
# Case 1: No parallel processing
############################################## 

# The Case 1 code has been commented out to save time. The
# timing information for this case was as follows:
# 
# user  system elapsed 
# 123.57    0.75  124.47 
# 
# p.time.Uniprocessing <- system.time({
#           my.svm.model <- train(y ~ .,
#                                 data = my.df,
#                                 method = "svmRadial",
#                                 preProc = c("center", "scale"),
#                                 tuneGrid = svmTuneGrid,
#                                 trControl = trainControl(method = "repeatedcv",
#                                                          repeats = 5,
#                                                          classProbs =  TRUE))
# })
# 
# final.svm.model <- svm(y ~ ., data=my.df,
#                        scale = T,
#                        kernel ="radial",
#                        cost=my.svm.model$bestTune[2],
#                        sigma=my.svm.model$bestTune[1],
#                        probability = T)
# #Evaluate the model against the test set
# pred.svm <- predict(final.svm.model, probability = T)
# my.svm.table <- table("Actual"=y,"Predicted"=pred.svm)
# round(my.svm.table / sum(my.svm.table) * 100,1)
# plot(x1, x2, col=colorsForPlot, pch=19, cex=1.05, xlab='x1', ylab='x2', main='Model Results' )

############################################## 
# Case 2: With parallel processing
############################################## 

registerDoParallel(cores=8)

p.time.Multiprocessing <- system.time({
  my.svm.model <- train(y ~ .,
                        data = my.df,
                        method = "svmRadial",
                        preProc = c("center", "scale"),
                        tuneGrid = svmTuneGrid,
                        trControl = trainControl(method = "repeatedcv", 
                                                 repeats = 5, 
                                                 classProbs =  TRUE))
})

final.svm.model <- svm(y ~ ., data=my.df, 
                       scale = T, 
                       kernel ="radial", 
                       cost=my.svm.model$bestTune[2], 
                       sigma=my.svm.model$bestTune[1],
                       probability = T)
#Evaluate the model against the test set
pred.svm <- predict(final.svm.model, probability = T)
my.svm.table <- table("Actual"=y,"Predicted"=pred.svm)
round(my.svm.table / sum(my.svm.table) * 100,1)
plot(x1, x2, col=colorsForPlot, pch=19, cex=1.05, xlab='x1', ylab='x2', main='Model Results' )
p.time.Multiprocessing
# p.time.Uniprocessing

