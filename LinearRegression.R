## -------------------------------------------------------------------------------
library(MASS)
library(ISLR2)


## -------------------------------------------------------------------------------
head(Boston)


## -------------------------------------------------------------------------------
names(Boston)


## -------------------------------------------------------------------------------
# Fit a simple linear regression model
lpm_fit <- lm(medv ~ lstat, data = Boston)
lpm_fit


## -------------------------------------------------------------------------------
summary(lpm_fit) # produces a much more informative output


## -------------------------------------------------------------------------------
# the components of an 'lm' objects are
names(lpm_fit)


## -------------------------------------------------------------------------------
confint(lpm_fit)


## -------------------------------------------------------------------------------
# lets predict the outcomes with new values for 'lstat' with confidence intervals
predict(lpm_fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")


## -------------------------------------------------------------------------------
# lets predict the outcomes with new values for 'lstat' with prediction intervals
predict(lpm_fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")


## -------------------------------------------------------------------------------
plot(Boston$lstat, Boston$medv, xlab = "lstat", ylab = "medv", col = "blue")
abline(lpm_fit, lwd = 2, col = "red")


## -------------------------------------------------------------------------------
# set up the plot area in a 2x2 configuration and plot diagnostics 
par(mfrow = c(2,2))
plot(lpm_fit)


## -------------------------------------------------------------------------------
plot(predict(lpm_fit), residuals(lpm_fit))


## -------------------------------------------------------------------------------
plot(hatvalues(lpm_fit))


## -------------------------------------------------------------------------------
lpm_fit2 <- lm(medv ~ lstat + age, data = Boston)
summary(lpm_fit2)


## -------------------------------------------------------------------------------
# R has a cool way to include all your variables without having to list them
lpm_fit3 <- lm(medv ~ ., data = Boston)


## -------------------------------------------------------------------------------
summary(lpm_fit3)


## -------------------------------------------------------------------------------
# We can update our model to remove 'indus' since it has a very high p-value
lpm_fit3 <- update(lpm_fit3, ~. - indus)
summary(lpm_fit3)


## -------------------------------------------------------------------------------
summary(lm(medv ~ lstat * age, data = Boston))


## -------------------------------------------------------------------------------
# Since ^ has a special meaning in an R formula, we use I() to make a transformation
# the formula (a+b)^2 produces a + b + a:b which is the same as a*b 
# X^2 is just X because there is nothing to cross X with

summary(lm(medv ~ lstat + I(lstat^2), data = Boston))

