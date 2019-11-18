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

# Splines #

needed <- c('ISLR', 'splines')   
installIfAbsentAndLoad(needed)

guac <- read.csv("avocado.csv", header = T)
attach(guac)
plot(AveragePrice, Large.Bags, cex=.5, col="darkgrey", ylim = range(Large.Bags)/10)
title("Smoothing Spline")
fit1 <- smooth.spline(AveragePrice, Large.Bags, cv=TRUE) # predict large bags on avg price
fit1$df
lines(fit1, col="blue", lwd=2)
legend("topright", legend="CV DF", col="blue", lty=1, lwd=2, cex=.8)

# Local Regression #

guaclims = range(AveragePrice)
guac.grid = seq(from=guaclims[1], to=guaclims[2], by = 0.001)
plot(AveragePrice, Large.Bags, xlim=guaclims, cex=.5, col="darkgrey", ylim = range(Large.Bags)/10)
title("Local Regression")
fit = loess(Large.Bags ~ AveragePrice, span =.2, data=guac) # predict large bags based on avg price
fit2 = loess(Large.Bags ~ AveragePrice, span =.5, data=guac)
lines(guac.grid, predict(fit, data.frame(AveragePrice = guac.grid)), 
      col="red", lwd=2)
lines(guac.grid, predict(fit2, data.frame(AveragePrice=guac.grid)),
      col="blue", lwd=2)
legend("topright", legend=c("Span = 0.2", "Span=0.5"),
       col=c("red", "blue"), lty=1, lwd=2, cex=.8)
