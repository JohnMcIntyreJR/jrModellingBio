## ----include = FALSE-----------------------
library(tufte)
knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ---- echo = TRUE, message= FALSE, warning = FALSE----
library("jrModelling")
library("broom")
library("tidyverse")

## ---- message = FALSE, warning = FALSE, echo = TRUE----
a = c(78.64,79.01, 79.57, 79.52, 80.71, 79.95, 78.50,
  79.10, 81.98, 80.09, 80.29, 80.22)
b = c(81.92, 81.12, 82.47, 82.86, 82.89, 82.45,
     82.51, 81.11, 83.07, 82.77, 82.38, 83.14)
results = tibble(a, b)

## ---- echo = TRUE--------------------------
 results = gather(results, key = "method", value = "value")

## ---- fig.keep = 'none'--------------------

# Box plot
ggplot(results, aes(x = method, y = value)) + 
  geom_boxplot()

# Density plot
ggplot(results, aes(x = value, fill = method)) + 
  geom_density(alpha = 0.4)

# QQ plots
ggplot(results, aes(sample = value, col = method)) + 
  geom_qq() + 
  geom_qq_line()

# Means and standard deviations
results %>%
  group_by(method) %>%
    summarise(mean = round(mean(value),2 ),
              sd = round(sd(value), 2))

## ------------------------------------------
t.test(value ~ method, data = results, var.equal=FALSE)

## ------------------------------------------
t.test(value ~ method, data = results, var.equal=TRUE)

## ------------------------------------------
wilcox.test(value ~ method, data = results)

## ---- echo = TRUE--------------------------
zodiac = c(348, 353, 359, 357, 350, 355, 359, 367, 345, 362, 343, 367)

## ------------------------------------------
zodiac = c(348, 353, 359, 357, 350, 355, 359, 367, 345, 362, 343, 367)
m = chisq.test(zodiac)
## Since p > 0.05 we can't accept the alternative hypothesis.
## However, the question is worded as though we can "prove" the Null
## hypotheis, which we obviously can't do.

## ---- message = FALSE, warning = FALSE-----
library("broom")
m_aug = augment(m)
m_aug$.expected

## ------------------------------------------
m_aug$.stdres

## ---- echo = TRUE--------------------------
tattoo = tibble(hep = c(17, 8, 22), no_hep = c(35, 53, 491))

## ---- warning=FALSE------------------------
    m = chisq.test(tattoo)

## ------------------------------------------
m_aug = augment(m)
m_aug$.expected

## ------------------------------------------
## Look at the .stdres column of aug_m. Some residuals are very large.
## We could consider combining cells.

## ---- echo = TRUE, eval = FALSE------------
#  vignette("solutions1", package = "jrModelling")

