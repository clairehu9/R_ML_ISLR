# In this example, num_awards is the outcome variable and
# indicates the number of awards earned by students at a
# high school in a year, math is a continuous predictor
# variable and represents students’ scores on their math
# final exam, and prog is a categorical predictor variable
# with three levels indicating the type of program in which
# the students were enrolled. It is coded as 1 = “General”,
# 2 = “Academic” and 3 = “Vocational”.
p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
summary(p)
hist(p$num_awards)
str(p)
p$id <- factor(p$id)
p$prog <- factor(p$prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
n <- nrow(p)
train  <-  sample(1:n, 0.75 * n)
m1 <- glm(num_awards ~ prog + math, family="poisson", data=p, subset=train)
summary(m1)
# The summary() output begins with echoing the function call. 

# The information on deviance residuals is displayed next. 
# Deviance residuals are approximately normally distributed 
# if the model is specified correctly. In our example, it 
# shows a little bit of skeweness since median is not quite 
# zero. 

# Next come the Poisson regression coefficients for each of 
# the variables along with the standard errors, z-scores, 
# p-values and 95% confidence intervals for the 
# coefficients. The coefficient for math is .063. This means
# that the expected log count for a one-unit increase in 
# math is .07. The indicator variable progAcademic compares
# prog = “Academic” and prog = “General”, the expected log
# count for prog = “Academic” increases by about 1.64. The 
# indicator variable prog.Vocational is the expected 
# difference in log count ((approx .81)) between prog = 
# “Vocational” and the reference group (prog = “General”).
 
# Because the coefficients are  the log of the fit -
# interpretation is easier if we take the exp of these
exp(cbind(coef(m1), confint(m1)))
# Here we see that, for example, a unit change in math leads
# to a change in the number of awards of approximately 1.07.

# The information on deviance is also provided. We can use
# the residual deviance to perform a goodness of fit test
# for the overall model. The residual deviance is the
# difference between the deviance of the current model and
# the maximum deviance of the ideal model where the 
# predicted values are identical to the observed. Therefore,
# if the residual deviance is small enough, the goodness 
# of fit test will not be significant, indicating that the 
# model fits the data. 
(p.value <- pchisq(150.14, 146))
# We conclude that the model fits reasonably well because 
# the goodness-of-fit chi-squared test is not statistically 
# significant. 

# If the test had been statistically significant, it would
# indicate that the data do not fit the model well. In that
# situation, we may try to determine if there are omitted
# predictor variables, if our linearity assumption holds
# and/or if there is an issue of over-dispersion. In the
# case of over-dispersion, we would consider a negative
# binomial regression (glm.nb() function in the MASS
# package).
# For Poisson regression the coefficients are the log of the
# fit - we need to take the exp of these

# Predict on the test set
preds <- predict.glm(m1, newdata=p[-train, ], type='response')
table('actuals'=p[-train, 2], 'prediction'=floor(preds))
