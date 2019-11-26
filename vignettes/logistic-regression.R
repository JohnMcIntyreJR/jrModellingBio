## ---- message = FALSE----------------------
library("titanic")
library("dplyr")
titanic_train %>%
  select(Age, Sex, Survived) %>%
  head(3)

## ---- label = "default-lm", fig.margin = TRUE, echo = FALSE, fig.cap = "Estimated probabilities of default using simple linear regression. Some probabilities are negative!", fig.height = 4----
x = titanic_train$Fare
plot(x, titanic_train$Survived,
     ylab = "Default probability", xlab = "Passenger Fare",
     pch = 21, bg = 1, panel.first = grid(), cex = 0.8)
abline(lm(Survived ~ Fare, data = titanic_train), col = 2, lwd = 2)

## ---- label = "log1", fig.margin = TRUE, echo = FALSE, fig.cap = "Estimated probabilities of default using a logistic regression model.", fig.height = 4----
m = glm(Survived ~ Fare, family = binomial, data = titanic_train)
plot(x, titanic_train$Survived,
     ylab = "Default probability", xlab = "Passenger Fare",
     pch = 21, bg = 1, panel.first = grid(), cex = 0.8)
p = predict(m, data.frame(Fare = 0:500), type = "response")
lines(0:500, p, col = 2)

## ---- label = "log2", fig.margin = TRUE, echo = FALSE, fig.cap = "The logistic function."----
x = seq(-6, 6, 0.01)
logisticfunc = function(x) {
  exp(x) / (1 + exp(x)) 
  }
plot(x, logisticfunc(x), type = "l", xlab = "x", main = "Logistic function", 
     panel.first = grid(), col = 4, ylab = expression(pi(x)))
#, with $\beta_0=0$ and $\beta_1=1$.}

## ------------------------------------------
library("titanic")
m = glm(Survived ~ Sex + Age, 
        family = binomial, 
        data = titanic_train)

## ------------------------------------------
library("broom")
tidy(m)

## ------------------------------------------
(g = glance(m))

## ------------------------------------------
1 - pchisq(g$deviance, g$df.residual)

## ------------------------------------------
1 - pchisq(g$null.deviance, g$df.null)

## ---- messgage= FALSE----------------------
confint(m)

## ------------------------------------------
exp(coef(m))

## ------------------------------------------
predict(m, 
        data.frame(Age = 50, Sex = c("male", "female")), 
        type = "response")

