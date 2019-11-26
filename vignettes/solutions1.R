## ----include = FALSE-----------------------
library(tufte)
# knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ---- echo = TRUE, message= FALSE, warning = FALSE----
library("jrModellingBio")
library("broom")
library("tidyverse")

## ------------------------------------------
data(two_classes, package = "jrModellingBio")

## ---- echo = TRUE--------------------------
two_classes_long = pivot_longer(two_classes, 
                            cols = c(CYT, EXC), 
                            names_to = "class", 
                            values_to = "mcg")

## ---- fig.keep = 'none'--------------------
# Box plot
ggplot(two_classes_long, aes(x = class, y = mcg)) +
  geom_boxplot()

# Density plot
ggplot(two_classes_long, aes(x = mcg, fill = class)) + 
  geom_density(alpha = 0.4)

# QQ plots
ggplot(two_classes_long, aes(sample = mcg)) + 
  geom_qq() + 
  geom_qq_line()

# Means and standard deviations
two_classes_long %>%
  group_by(class) %>%
  summarise(mean = round(mean(mcg), 2),
            sd = round(sd(mcg), 2))

## ------------------------------------------
t.test(mcg ~ class, data = two_classes_long, var.equal = FALSE)

## ------------------------------------------
t.test(mcg ~ class, data = two_classes_long, var.equal = TRUE)

## ---- message=FALSE------------------------
wilcox.test(mcg ~ class, data = two_classes_long)

## ---- echo = TRUE--------------------------
data(yeast_classes, package = "jrModellingBio")

## ------------------------------------------
m = chisq.test(yeast_classes)
## Since p < 0.05 we can reject the null hypothesis.
## We have strong evidence that yeast proteins are distributed uniformly across subcellular localisations. 

## ---- message = FALSE, warning = FALSE-----
library("broom")
m_aug = augment(m)
m_aug$.expected

## ------------------------------------------
m_aug$.stdres

## ---- echo = TRUE--------------------------
data(yeast, package = "jrModellingBio")

## ---- warning=FALSE------------------------
test = cor.test(~ mcg + gvh, data = yeast)
test

## ------------------------------------------
r2 = signif(glance(test)$estimate, 3)

## ------------------------------------------
ggplot(yeast, aes(x = mcg, y = gvh)) +
  geom_point() +
  annotate("label", x = 0.1, y = 0.9, label = paste("r2 = ", r2), hjust = 0)

## ---- echo = TRUE, eval = FALSE------------
#  vignette("solutions1", package = "jrModellingBio")

