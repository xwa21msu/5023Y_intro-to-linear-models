#________________----

# PACKAGES ----

library(tidyverse)
library(broom)
library(ggplot2)
library(ggridges)

#________________----

# CLEAR ENVIRONMENT ----

rm(list = ls())

#________________----


# HYPOTHESES ----

# Hypothesis one: the effect of sexual activity on longevity
# Possible interacting factors on sexual activity that may affect longevity: sleep:type
# Main Effects: thorax (on longevity), sleep (on longevity), type

#________________----

# DATA ----

# import ----
fruitfly <- read_csv(here::here("data", "fruitfly.csv"))

head(fruitfly)

colnames(fruitfly)

glimpse(fruitfly)

# check for duplicated data ----
fruitfly %>% 
  duplicated() %>% 
  sum() # 0

# check for missing values ----
fruitfly %>% 
  is.na() %>% 
  sum() # 0

# check for typos by looking at distinct characters/values ----
fruitfly %>% 
  distinct(type)

# check for typos - by looking at impossible values ----
fruitfly %>% 
  summarise(min=min(longevity, na.rm=TRUE), 
            max=max(longevity, na.rm=TRUE))

fruitfly %>% 
  summarise(min=min(thorax, na.rm=TRUE), 
            max=max(thorax, na.rm=TRUE))

fruitfly %>% 
  summarise(min=min(sleep, na.rm=TRUE), 
            max=max(sleep, na.rm=TRUE))

# quick summary ----
summary(fruitfly)

#________________----

# DATA VISUALISATION ----
# overview ----
GGally::ggpairs(fruitfly)

# density distribution for longevity of males across three treatments ----
colours <- c("cyan", "darkorange", "purple")

fruitfly %>% 
  ggplot(aes(x = longevity, y = type, fill = type))+
  geom_density_ridges(alpha = 0.5)+
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(legend.position = "none")

# looks like type affects longevity

# scatter plot (and interaction plot) of size against longevity ----
fruitfly %>%
  ggplot(aes(x = thorax, y = longevity, colour = type)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = colours)+
  geom_smooth(method="lm", se=FALSE, aes(colour=type))+
  theme_minimal()

# looks like size affects longevity
# size affecting longevity doesnt seem to change between groups

# interaction plot of sleep on treatment (and therefore longevity)
fruitfly %>% 
  ggplot(aes(x=sleep, y = longevity, group = type, colour = type))+
  geom_point( alpha = 0.6)+
  geom_smooth(method = "lm",
              se = FALSE)+
  scale_colour_manual(values = colours)+
  theme_minimal()
# looks like sleep interacts with type and therefore longevity.
# would need to model it to test the association and if it may be observed under the null.

#________________----

# DESIGNING A MODEL ----
# a full model
flyls1 <- lm(longevity ~ type + thorax + sleep + type:sleep, data = fruitfly)

flyls1 %>% 
  broom::tidy()
# interaction line = how much more or less mean estimate is than if the main effects were just combined.
# doesnt appear the interaction is having a strong effect compared to the main effect

# mean longevity of a male with 0.79mm thorax, sleeps 22% of day and paired with virgin females:
# intercept
coef(flyls1)[1] + 
  
  # 1*coefficient for virgin treatment  
  coef(flyls1)[3] + 
  
  # 0.79 * coefficient for thorax size  
  (coef(flyls1)[4]*0.79) + 
  
  # 22 * coefficient for sleep  
  (coef(flyls1)[5]*22)
  
  # 22 * 1 * coefficient for interaction
  (coef(flyls1)[7]*22*1)

#________________----

# MODEL CHECKING AND COLINEARITY ----
# check the model is a good fit before proceeding.

performance::check_model(flyls1)

car::vif(flyls1)
# measure of SE of each estimated coefficient
# greater than 5 or 10 then the model is having issues estimating
# doesnt affect predictions, but more difficult to determine the estimate change from a predictor

#________________----

# DATA TRANSFORMATIONS ----
# when response variable is non-normal (violates model assumptions)

# boxcox ----
MASS::boxcox(flyls1)
# maximum likelihood curve for the best transformation for fitting the data.
# looks like it may work?:

flyls_sqrt <- lm(sqrt(longevity) ~ type + thorax + sleep + type:sleep, data = fruitfly)

performance::check_model(flyls_sqrt)
# in actuality a sqrt doesn't help - may as well stick with original dataset

#________________----

# MODEL SELECTION ----
# use drop1 function to remove top-level terms ----
drop1(flyls1, test = "F")
# F is not significantly different for interaction term -> can be dropped.

# interaction dropped model 2 ----
flyls2 <- lm(longevity ~ type + thorax + sleep, data = fruitfly)

drop1(flyls2, test = "F")
# AIC isnt improved by removing sleep (AIC is an estimator of model error and therefore quality)

#________________----

# POST-HOC ----
emmeans::emmeans(flyls2, specs = pairwise ~ type + thorax + sleep)

