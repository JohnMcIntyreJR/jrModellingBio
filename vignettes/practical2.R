## ----include = FALSE-----------------------
library(tufte)
knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ---- echo = TRUE--------------------------
library("jrModellingBio")
library("broom")
library("tidyverse")

## ---- echo = TRUE--------------------------
data(yeast, package = "jrModellingBio")

## ------------------------------------------
m = lm(mcg ~ gvh, data = yeast)
tidy_m = tidy(m)
tidy_m
##The p-value for the gradient is 0.01
##This suggests gvh is useful in explaining variation in 

## ----F1, fig.keep='none', message = FALSE, warning = FALSE----
library("ggplot2")
ggplot(yeast, aes(x = gvh, y = mcg)) +
    geom_point() +
    geom_abline(intercept = tidy_m$estimate[1],
                    slope = tidy_m$estimate[2],
                linetype = 2, colour = "red")

## ----fig.margin = TRUE, fig.cap="Scatterplot with the earnings data. Also shows the line of best fit.", out.width='\\textwidth', echo=FALSE----
ggplot(yeast, aes(x = gvh, y = mcg)) +
    geom_point() +
    geom_abline(intercept = tidy_m$estimate[1],
                    slope = tidy_m$estimate[2],
                linetype = 2, colour = "red") +
    theme_bw()

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

## ------------------------------------------
# When we fit a regression model we are implicitly assuming a causal relationship.
# In this case we are talking about two measurements of signal peptides, one is unlikely to be caused by the other but they are correlated because some (as yet) unseen variable is causally related to both.

## ---- echo=TRUE----------------------------
data(outbreaks, package = "jrModellingBio")

## ----fig.keep='none'-----------------------
m = lm(illnesses ~ year, data = outbreaks)
m_aug = augment(m)
ggplot(m_aug, aes(sample = .std.resid)) +
    geom_qq() +
    geom_qq_line(colour = "steelblue",
                linetype = 2)
# This looks way off, sensible first step here is to transform the y variable. Lets try a log transformation.

## ----fig.keep='none'-----------------------
m = lm(log(illnesses) ~ year, data = outbreaks)
m_aug = augment(m)
ggplot(m_aug, aes(sample = .std.resid)) +
    geom_qq() +
    geom_qq_line(colour = "steelblue",
                linetype = 2)
# Not perfect but much better, fine for our purposes.

## ------------------------------------------
m = lm(log(illnesses) ~ year * species, data = outbreaks)

## ------------------------------------------
glance(m)
tidy(m)
# r2 is 0.06 so nearly 6% of the variation in log illnesses is captured by this model
# Salmonella is on the increase

## ----fig.keep='none'-----------------------
ggplot(outbreaks, aes(x = year, y = log(illnesses))) +
    geom_smooth(aes(colour = species), method = "lm")

## ----fig.margin = TRUE, fig.cap="Linear model of log(illnesses) against year and species with an interaction term", out.width='\\textwidth', echo=FALSE----
ggplot(outbreaks, aes(x = year, y = log(illnesses))) +
    geom_smooth(aes(colour = species), method = "lm")

## ------------------------------------------
m = aov(log(illnesses) ~ state, data = outbreaks)
tidy_m = tidy(m)
tidy_m    
## The p value is small.
## This suggests a difference may exist.

## ------------------------------------------
TukeyHSD(m)
# California has a higher incidence of illnesses while the other two are equal. 

## ---- fig.keep="none"----------------------
ggplot(outbreaks, aes(x = state, y = log(illnesses))) +
    geom_boxplot(aes(fill = state))

ggplot(outbreaks, aes(x = log(illnesses))) +
    geom_density(aes(fill = state), alpha = 0.5)

## ----fig.margin = TRUE, fig.cap="Distribution of log(illnesses) across three states", out.width='\\textwidth', echo=FALSE----
ggplot(outbreaks, aes(x = log(illnesses))) +
    geom_density(aes(fill = state), alpha = 0.5)

## ---- echo = TRUE--------------------------
data(yeast, package = "jrModellingBio")
# restrict the data two two localisations
yeast = filter(yeast, class == "EXC" | class == "ME1")

## ---- echo = TRUE--------------------------
## extract the sequence labels of localisations
seq_localisations = yeast$class
yeast = yeast %>% select(mcg, gvh, alm, mit, vac, nuc)

## ------------------------------------------
clusters = hclust(dist(yeast))

## ---- fig.keep="none"----------------------
plot(hclust(dist(yeast)), labels = seq_localisations)

## ---- echo=TRUE----------------------------
library("tidygraph")
library("ggraph")

## ------------------------------------------
cluster_tbl = as_tbl_graph(clusters)

## ---- echo=TRUE----------------------------
seq_labels = tibble(
    label = factor(seq_along(seq_localisations)),
    localisations =  seq_localisations
) 

## ------------------------------------------
cluster_tbl = cluster_tbl %>% 
    activate(nodes) %>% 
    left_join(seq_labels)

## ---- fig.keep='none'----------------------
ggraph(cluster_tbl, 'tree') + 
    geom_edge_elbow(width = 0.2) +
    geom_node_point(aes(filter = leaf, colour = localisations), size = 2)

## ----fig.margin = TRUE, fig.cap="Dendrogram of yeast proteins clustered by similarity in amino acid sequence properties", out.width='\\textwidth', echo=FALSE----
ggraph(cluster_tbl, 'tree') + 
    geom_edge_elbow(width = 0.2) +
    geom_node_point(aes(filter = leaf, colour = localisations), size = 2)

## ------------------------------------------
##Round to 2dp
signif(cor(yeast), 2)

## ------------------------------------------
##Run principle components
prcomp(yeast, scale = TRUE)

## ----  fig.keep="none"---------------------
biplot(prcomp(yeast, scale = TRUE))

## ---- echo = TRUE, eval = FALSE------------
#  vignette("solutions2", package = "jrModelling")

