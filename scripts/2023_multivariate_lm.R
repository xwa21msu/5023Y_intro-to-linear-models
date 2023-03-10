#________________----

# PACKAGES ----

library(tidyverse)
library(broom)

#________________----

# CLEAR ENVIRONMENT ----

rm(list = ls())

#________________----

# DATA ----

biomass <- read_csv(here::here("data", "biomass.csv"))

head(biomass)

biomass <- janitor::clean_names(biomass)

colnames(biomass)

glimpse(biomass)

# check for duplicated data ----
biomass %>% 
  duplicated() %>% 
  sum() # 0

# check for missing values ----
biomass %>% 
  is.na() %>% 
  sum() # 0

# check for typos by looking at distinct characters/values ----
biomass %>% 
  distinct(fert)

biomass %>% 
  distinct(light)

biomass %>% 
  distinct(fl)

# check for typos - by looking at impossible values ----
biomass %>% 
  summarise(min=min(biomass_m2, na.rm=TRUE), 
            max=max(biomass_m2, na.rm=TRUE))

# quick summary ----
summary(biomass)

#________________----

# DATA SUMMARY ----

# use col3 only = one-way-anova
# use col1&2 = factorial design

#________________----

# ONE-WAY-ANOVA ----
# only after making a boxplot to determine a real difference in means across treatments.

# fl column only = four level one-way-anova

ls_1 <- lm(biomass_m2 ~ fl, data = biomass)
summary(ls_1)

# confidence intervals ----
broom::tidy(ls_1, conf.int = T)

# coefficient plot ----
GGally::ggcoef_model(ls_1,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)
# adding light alone makes biomass increase but the CIs include 0 -> little confidence that this is a true effect.

# looking at the fourth level (combined f and l) ---- 
# combine the average mean differences of the light effect and fertiliser effect
coef(ls_1)[2] + coef(ls_1)[3]

# compare this to the average difference of the combined treatment
coef(ls_1)[4]

# combined is larger than adding them together individually
# could be a positive interaction between f and l on biomass

# dont know the magnitude of effect - factorial tests compare additive effects and their interaction

#________________----

# TESTING FOR INTERACTIONS ----

biomass %>% ggplot(aes(x=fert, y=biomass_m2, colour = light, fill = light, group = light))+
  geom_jitter(width=0.1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 23
  )+stat_summary(
    geom = "line",
    fun = "mean",
    size = 1, linetype = "dashed"
  )


ls_2 <- lm(biomass_m2 ~ fert + # main effect
             light + # main effect
             fert:light, # interaction term
           data = biomass)
# need to separate out the factors instead of using 'fl'
summary(ls_2)
# the fourth line now indicates how much of an effect the two factors interacting changes the mean
# the SE is larger -> less power to estimate an interaction over main effect

# coefficient  plot ----
GGally::ggcoef_model(ls_2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)
# if combining f and l made a biomass = to additive predictions, interaction estimate = 0
# actually = 95g, so need to sum additive effects of f & l and f:l.

# model 1
coef(ls_1)[4]

# model 2
coef(ls_2)[2] + coef(ls_2)[3] + coef(ls_2)[4]
# adding the f,l and f:l together makes them add up to the same effect of fl.


