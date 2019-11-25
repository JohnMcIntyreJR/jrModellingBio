## ----include = FALSE-----------------------
library(tufte)
knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ---- echo = TRUE--------------------------
library("jrModelling")
library("broom")
library("ggplot2")

## ---- echo = TRUE--------------------------
data(icecream, package = "jrModelling")

## ------------------------------------------
m = lm(sales ~ temperature, data = icecream)
tidy_m = tidy(m)
tidy_m
##The p-value for the gradient is 9.9e-09
##This suggests temperature is useful

## ------------------------------------------
cor(icecream$temperature, icecream$sales)

## ----F1, fig.keep='none', message = FALSE, warning = FALSE----
library("ggplot2")
ggplot(icecream, aes(x = temperature, y = sales)) +
    geom_point() +
    geom_abline(intercept = tidy_m$estimate[1],
                    slope = tidy_m$estimate[2],
                linetype = 2, colour = "red") + 
    labs(x = "Temp", y = "Sales")

## ----fig.margin = TRUE, fig.cap="Scatterplot with the earnings data. Also shows the line of best fit.", out.width='\\textwidth', echo=FALSE----
    ggplot(icecream, aes(x = temperature, y = sales)) +
        geom_point() +
        geom_abline(intercept = tidy_m$estimate[1],
                        slope = tidy_m$estimate[2],
                    linetype = 2, colour = "red") + 
        labs(x = "Temp", y = "Sales") + 
    annotate("label", x = 6, y = 125, label = "r = 0.983") + 
    theme_bw()

## ---- fig.keep='none', eval = FALSE--------
#  r = round(cor(icecream$temperature, icecream$sales), 2)
#  ggplot(icecream, aes(x = temperature, y = sales)) +
#      geom_point() +
#      geom_abline(intercept = tidy_m$estimate[1],
#                      slope = tidy_m$estimate[2],
#                  linetype = 2, colour = "red") +
#      labs(x = "Temp", y = "Sales") +
#  annotate("label", x = 6, y = 125, label = paste("r =", r))

## ----F2, fig.keep='none', message = FALSE, warning = FALSE----
##Model diagnosics look good
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
    geom_qq_line(colour = "steelblue",
                linetype = 2) 
##Model diagnosics look good

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

## ---- echo = TRUE--------------------------
data(drphil, package = "jrModelling")

## ------------------------------------------
(m = lm(IQ ~ AgeBegin + AgeEnd + TotalYears, data = drphil))
#The problem is TotalYears = AgeEnd - AgeBegin
#Solution: remove TotalYears

## ---- echo = TRUE--------------------------
data(iq, package = "jrModelling")

## ------------------------------------------
m = aov(score ~ music, data = iq)
tidy_m = tidy(m)
tidy_m    
## The p value is around 0.056.
## This suggests a difference may exist.

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

# The p values indicate that there the main differences can be found between the Mozart - Heavy Metal & Silence - Heavy Metal comparisons. Looking at the boxplot we can see that the iq scores for participants listening to Heavy Metal were lower than those listening to Mozart or silence. However, there was not much difference in performance between those listening to Mozart compared to those listening to silence.

ggplot(iq, aes(x = music, y = score)) +
    geom_boxplot()

## ---- echo = TRUE--------------------------
data(hep, package = "jrModelling")
##Remove the athletes names and final scores.
hep_names = hep[, 1]
hep = hep[, 2:8]

## ---- fig.keep="none"----------------------
plot(hclust(dist(hep)), labels = hep_names)

## ------------------------------------------
##Round to 2dp
signif(cor(hep), 2)

## ------------------------------------------
##Run principle components
prcomp(hep)

## ------------------------------------------
##Yes!. run800m dominates the loading since
##the scales differ
prcomp(hep, scale = TRUE)

## ----  fig.keep="none"---------------------
biplot(prcomp(hep, scale = TRUE))

## ---- echo = TRUE, eval = FALSE------------
#  vignette("solutions2", package = "jrModelling")

