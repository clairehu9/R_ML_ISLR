rm(list=ls())

library(splines)
library(boot)
nba <- read.csv("nba_value.csv", as.is = T)
attach(nba)

###### Exercise 1 #######

# 1. Use bs() to fit a regression spline to predict VALUE using AVG. 
#    Hint:(1) Use cubic spline to fit each region
#         (2) Use only 1 knot here and choose the location on your own


# 2. Calculate the MSE


# 3. Plot the resulting fit and also the vertical line of where the knot is
#    Hint: make sure to give the plot a title


###### Exercise 2 #######

# 1. Use bs() to fit a regression spline again and choose the number of knots and also the locations
#    Hint: use cubic spline to fit each region


# 2. How did you choose the # of knots and where were the knots placed in the previous question? 

# 3. Calculate the MSE
#    Hint: If you could generate a MSE that is lower than us and use less 
#          or equal number of knots as we did (which are 4 knots), you would get some liitle prizes

# 4. Plot the resulting fit and also the vertical lines of where these knots are

# 5. Explain why the MSE is high for this dataset

###### Exercise 3 #######

# 1. Use bs() and ns() to fit a regression spline for a range of degrees of freedom 
#    Hint:you will decide the range of df on your own 

# 2. Plot the resulting fits

# 3. Report the associated MSE in a table

# 4. Set seed to 5072

# 5. Do cross-validation to decide the optimal number of knots 

# 6. Report the associated knots and MSEs in a table

# 7. Plot the results 