## ----include = FALSE-----------------------
library(tufte)
# knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ------------------------------------------
x = c(4,4,7,8,12,15,16,17,14,11,7,5)
y = c(73, 57, 81, 94, 110, 124, 134, 139, 124, 103, 81, 80)
m = lm(y~x)
summary(m)
##The p-value for the gradient is 9.9e-09
##This suggests temperature is useful

## ------------------------------------------
cor(x, y)

## ----F1, fig.keep='none', message = FALSE, warning = FALSE----
library("ggplot2")
library("tibble")
df = tibble(x = x, y = y)
ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(
        method = "lm", se = FALSE,
        colour = "red", linetype = 2) + 
    labs(x = "Temp", y = "Sales")

## ----fig.margin = TRUE, fig.cap="Scatterplot with the earnings data. Also shows the line of best fit.", out.width='\\textwidth', echo=FALSE----
ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(
        method = "lm", se = FALSE,
        colour = "red", linetype = 2) + 
    labs(x = "Temp", y = "Sales") + 
    annotate("label", x = 6, y = 125, label = "r = 0.983") + 
    theme_bw()

## ---- fig.keep='none', eval = FALSE--------
#  ggplot(df, aes(x = x, y = y)) +
#  geom_point() +
#  geom_smooth(
#      method = "lm", se = FALSE,
#      colour = "red", linetype = 2) +
#  labs(x = "Temp", y = "Sales") +
#  annotate("label", x = 6, y = 125, label = paste("r =", r))

## ----F2, fig.keep='none', message = FALSE, warning = FALSE----
##Model diagnosics look good
library("broom")
m_aug = augment(m)
ggplot(m_aug, aes(x = .fitted, y = .std.resid)) +
    geom_point() +
    geom_hline(
        yintercept = c(0, -2, 2),
        linetype = c(2, 3, 3),
        colour = c("red", "green", "green")
    )

## ----fig.keep='none'-----------------------
ggplot(m_aug, aes(sample = .std.resid)) +
    geom_qq() +
    geom_abline(colour = "steelblue",
                linetype = 2) 
##Model diagnosics look good

## ----echo=FALSE----------------------------
x = c(4, 4, 7, 8, 12, 15, 16, 17, 14, 11, 7, 5)
y = c(73, 57, 81, 94, 110, 124, 134, 139, 124, 103, 81, 80)

## ------------------------------------------
m = lm(y~x)

## ------------------------------------------
cor(x, y)

## ----fig.keep='none'-----------------------
dfq2 = tibble(x = x, y = y)
ggplot(dfq2, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(
        method = "lm", se = FALSE,
        colour = "red", linetype = 2) 

## ----fig.keep='none'-----------------------
m_aug = augment(m)
ggplot(m_aug, aes(x = .fitted, y = .std.resid)) +
    geom_point() +
    geom_hline(
        yintercept = c(0, -2, 2),
        linetype = c(2, 3, 3),
        colour = c("red", "green", "green")
    )

## ----fig.keep='none'-----------------------
ggplot(m_aug, aes(sample = .std.resid)) +
    geom_qq() +
    geom_abline(colour = "steelblue",
                linetype = 2) 

## ---- echo = TRUE, message = FALSE---------
library("jrModelling")
data(graves)

## ----fig.keep='none'-----------------------
fit = lm(OI ~ age + Sex, data = graves)

## ----fig.keep='none'-----------------------
fit_aug = augment(fit)
ggplot(fit_aug, aes(x = OI,
                    y = .std.resid)) +
    geom_point() +
    geom_hline(yintercept = c(0, -2, 2),
               linetype = c(2, 3, 3),
               colour = c("red", "green", "green"))

ggplot(fit_aug, aes(x = age, 
                    y = .std.resid)) +
    geom_point() +
    geom_hline(yintercept = c(0, -2, 2),
               linetype = c(2, 3, 3),
               colour = c("red", "green", "green"))

ggplot(fit_aug, aes(x = .fitted, y = .std.resid)) +
    geom_point() + 
    geom_hline(yintercept = c(0,-2, 2), 
                 linetype = c(2,3,3), 
                 colour = c("red", "green", "green"))

ggplot(fit_aug, aes(sample = .std.resid)) +
    geom_qq() +
    geom_abline(colour = "steelblue",
                linetype = 2)

# The q-q plot shows the residuals lying close to the fitted straight 
# line which suggests that the normality assumption is satisfied.
# The residuals in the first plot appear to show a pattern. 
# Consider transforming the response variable or the explanatory variables
# or adding a square term / interaction term to your model.

## ------------------------------------------
data(drphil)

## ------------------------------------------
(m = lm(IQ ~ AgeBegin + AgeEnd + TotalYears, data=drphil))
#The problem is TotalYears = AgeEnd - AgeBegin
#Solution: remove TotalYears

## ------------------------------------------
x1 = c(109, 114, 108, 123, 115, 108, 114)
x2 = c(113, 114, 113, 108, 119, 112, 110)
x3 = c(103, 94, 114, 107, 107, 113, 107)
dd = tibble(values = c(x1, x2, x3), type = rep(c("M", "S", "H"), each=7))
m = aov(values ~ type, dd)
summary(m)
##The p value is around 0.056.
##This suggests a difference may exist.

## ----F3, fig.keep='none'-------------------
m_aug = augment(m)
ggplot(m_aug, aes(x = .fitted, y = .std.resid)) +
    geom_point() + 
    geom_hline(yintercept = c(0,-2, 2), 
                 linetype = c(2,3,3), 
                 colour = c("red", "green", "green"))
## Residual plot looks OK

## ----fig.margin = TRUE, out.width='\\textwidth', echo=FALSE, fig.cap = "Model diagnosics for the music data.", message = FALSE, warning = FALSE----
ggplot(m_aug, aes(x = .fitted, y = .std.resid)) +
    geom_point() + 
    geom_hline(yintercept = c(0,-2, 2), 
                 linetype = c(2,3,3), 
                 colour = c("red", "green", "green")) +
    ylim(-2.5, 2.5) +
    theme_bw() +
    labs(x = "Fitted values",
         y = "Standardised Residuals")

## ------------------------------------------
TukeyHSD(m)

## ---- echo = TRUE--------------------------
data(hep)
##Remove the athletes names and final scores.
hep_s = hep[,2:8]

## ----fig.keep="none"-----------------------
plot(hclust(dist(hep_s)), labels=hep[,1])

## ------------------------------------------
##Round to 2dp
signif(cor(hep_s), 2)

## ------------------------------------------
##Remove:
##1st column: athletes name
##Last column: It's a combination of the other columns
dd = hep[ ,2:8]

##Run principle components
prcomp(dd)

## ------------------------------------------
##Yes!. run800m dominates the loading since
##the scales differ

## ----  fig.keep="none"---------------------
prcomp(dd, scale=TRUE)
biplot(prcomp(dd, scale=TRUE))

## ---- eval = TRUE--------------------------
vignette("solutions2", package = "jrModelling")

