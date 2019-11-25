library(dplyr)
library(tidyr)
library(ggplot2)

load("data/yeast.RData")

yeast %>% 
  group_by(class) %>%
  summarise(avg_mcg = mean(mcg),
            avg_gvh = mean(gvh))

two_classes = yeast %>% 
  select(class, mcg) %>% 
  filter(class %in% c("CYT", "EXC")) %>%
  group_by(class) %>% 
  sample_n(35) %>%
  pivot_wider(names_from = class, values_from = mcg) %>%
  unnest(cols = c(CYT, EXC))

save(two_classes, file = "data/two_classes.RData")

