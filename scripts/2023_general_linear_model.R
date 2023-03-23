#________________----

# PACKAGES ----

library(tidyverse)
library(broom)
library(ggplot2)
library(ggridges)
library(GGally)
library(emmeans)
library(performance)

#________________----

# SOURCE LAST SCRIPT ----

source("scripts/2023_complex_models.R")

#________________----

# LM VS GLM ----

flyls3 <- lm(formula = longevity ~ sleep + type + thorax, data = fruitfly)
summary(flyls3)

flyglm <- glm(longevity ~ sleep + type + thorax, 
              family = gaussian(link = "identity"), 
              data = fruitfly)
summary(flyglm)

# lm and glm the same because of the probability of the model being fitted to an OLM if high with this data.

#________________----

# POISSON REGRESSION - CUCKOOS ----

#________________----

# DATA ----

cuckoo <- read_csv(here::here("data", "cuckoo.csv"))

head(cuckoo)

#________________----

# VISULAISATION ----

ggplot(cuckoo, aes(x=Mass, y=Beg, colour=Species)) + geom_point()

# appears there is a relationship between beg and mass and could be different between species
# tempting to fit a lm but doing so predicts negative begging calls (?) - no sense.

#________________----

# MODEL DIAGNOSTIC PLOTS FOR LM ----
## There is an interaction term here, it is reasonable to think that how calling rates change with size might be different between the two species.

# linear model ----
cuckoo_ls1 <- lm(Beg ~ Mass+Species+Mass:Species, data=cuckoo) 

# performance checks ----
performance::check_model(cuckoo_ls1, 
                         check = c("homogeneity",
                                   "qq"))
# strong funneling effect - assumptions violated.
# must try a poisson model using canonical log link function (due to being count data)

# fitting poisson glm ----
cuckoo_glm1 <- glm(Beg ~ Mass + Species + Mass:Species, data=cuckoo, family=poisson(link="log"))
summary(cuckoo_glm1)

# appears there is a negative interaction effect for interaction term
# shows begging calls do not increase with mass as much as you would expect for warblers compared to cuckoos.

# plotting regression for each species ----
# using augment allows you to generate fitted outcomes from the regression, make sure to set the predictions onto the response scale in order to 'back transform` the data onto the original scale
broom::augment(cuckoo_glm1, type.predict = "response") %>% 
  ggplot(aes(x=Mass, y=.fitted, colour=Species)) + 
  geom_point() +
  geom_line()+
  scale_colour_manual(values=c("green3","turquoise3"))+
  theme_minimal()

broom::augment(cuckoo_glm1) %>% 
  ggplot(aes(x=Mass, y=.fitted, colour=Species)) + 
  geom_point() +
  geom_line()+
  scale_colour_manual(values=c("green3","turquoise3"))+
  theme_minimal()
# if we fit model on log scale, get fitted glm response (y on log) - straight lines
# exponential curve when using the original data with type.predict = response (making it the same as the log transformed dataset)

# checking fit of poisson model ----
performance::check_model(cuckoo_glm1, 
                         check = c("homogeneity",
                                   "qq"))
summary(cuckoo_glm1)

#________________----

# ESTIMATES AND INTERVALS ----
# exponentiate the estimates to get them on the same scale as Y (all model estimates before this are logn(y))

exp(coef(cuckoo_glm1)[1]) ### Intercept - Incidence rate at Mass=0, and Species = Cuckoo

exp(coef(cuckoo_glm1)[2]) ### Change in the average incidence rate with Mass 

exp(coef(cuckoo_glm1)[3]) ### Change in the incidence rate intercept when Species = Warbler and Mass = 0

exp(coef(cuckoo_glm1)[4]) ### The extra change in incidence rate for each unit increase in Mass when Species = Warbler (the interaction effect)

broom::tidy(cuckoo_glm1, 
            exponentiate = T, 
            conf.int = T)
# tidying models with broom and specifying exponentiate = T puts model predictions on the response variable by removing log transformation

#________________----

# INTERPRETATION ----
# important to remember if you're using the log-link scale or original scale 

# For a fixed  mean-variance model we use a Chisquare distribution
drop1(cuckoo_glm1, test = "Chisq")

# emmeans can be another handy function - if you specify response then here it provideds the average call rate for each species, at the average value for any continuous measures - so here the average call rate for both species at an average body mass of 20.3
emmeans::emmeans(cuckoo_glm1, specs = ~ Species:Mass, type = "response")

#________________----

# OVERDISPERSION ----

# overdispersion = residual deviance/residual DFs
# from summary using poisson only (above) is 436/47 = 9.3
# overdispersed is >1 overdispersion statistic values.
# -> issue due to larger than expected variance for that mean under poisson distribution
# fixed with quasi-liklihood model (which accounts for the above)

cuckoo_glm2 <- glm(Beg ~ Mass+Species+Mass:Species, data=cuckoo, family=quasipoisson(link="log"))
summary(cuckoo_glm2)
# no estimates have changed but the error and CIs have
# accounts for greater than expected uncertainty seen with deviance
# interaction effect appears no longer significant with wider standard error

# now estimating with variance again (not deviance) so using t distribution and F tests with ANOVA and drop1()

#________________----

# LOGISTIC REGRESSION (binomial data) ----

#________________----

# DATA ----

challenger <- read_csv(here::here("data", "Challenger.csv"))

head(challenger)

#________________----

# VISUALISATION ----
# was noted that temperature might affect O-ring safety

# only looking at flights where a failure occurred: ----
challenger %>% 
  filter(oring_dt > 0) %>% 
  ggplot(aes(y=oring_dt, x=temp))+geom_point()+
  ggtitle("Temperature on flight launches where an O-ring incident occurred")

# concluded temp doesnt affect o-ring failures (due to occuring at different temps)

# looking at the whole dataset ----
challenger %>% 
  ggplot(aes(y=oring_dt, 
             x=temp))+
  geom_point()+
  geom_smooth(method="lm")+
  ggtitle("All launch data")

# clear relationship between temperature and o-ring failure

#________________----

# BINARY GLM ----

