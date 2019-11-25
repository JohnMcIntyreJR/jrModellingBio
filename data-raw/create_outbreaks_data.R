library(readr)
library(dplyr)
library(tidyr)

outbreaks = read_csv("data-raw/outbreaks.csv")

top3 = outbreaks %>% 
  drop_na(Species) %>% 
  group_by(Species) %>% 
  count(sort = TRUE) %>% 
  ungroup() %>% 
  top_n(3)

outbreaks = outbreaks %>% 
  filter(Species %in% top3$Species)

colnames(outbreaks) = tolower(colnames(outbreaks))

save(outbreaks, file = "data/outbreaks.RData")


# ggplot(outbreaks, aes(x = Illnesses)) +
#   geom_density(aes(fill = Species))
# 
# ggplot(outbreaks, aes(x = log(Illnesses))) +
#   geom_density(aes(fill = Species), alpha = 0.5)
# 
# ggplot(outbreaks, aes(y = log(Illnesses), x = Year)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   facet_wrap(~ Species)
# 
# 
# options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
# 
# mod = lm(log(Illnesses) ~ Species * Year, data = outbreaks)
# library(broom)
# tidy(mod)


