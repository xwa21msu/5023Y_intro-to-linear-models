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

# fitting poisson glm
cuckoo_glm1 <- glm(Beg ~ Mass + Species + Mass:Species, data=cuckoo, family=poisson(link="log"))
summary(cuckoo_glm1)

# appears there is a negative interaction effect for interaction term
# shows begging calls do not increase with mass as much as you would expect for warblers compared to cuckoos.

