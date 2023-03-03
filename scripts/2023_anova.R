#____________----

# PACKAGES ----
library(tidyverse)

#____________----

rm(list = ls())

# DATA ----

darwin <- read_csv(here("data", "darwin.csv"))

#____________----

# CHECK DATA----

#check data is in a tidy format ----
head(darwin)

colnames(darwin) #column names look good

#clean up column names ----
darwin <- janitor::clean_names(darwin)

glimpse(darwin)

#____________----

# LINEAR MODEL ----

lsmodel1 <- lm(height ~ type, data = darwin)

#____________----

# THE ANOVA TABLE ----

anova(lsmodel1)

summary(lsmodel1)

pf(5.9395, 1, 28, lower.tail=FALSE)
# The first three arguments here are the F-value, degrees of freedom for the signal, and noise. 
# The last argument is to set this as a two directional test.
# Big F value = SSR > SSE and the model is a good fit.

#____________----

# TWO-WAY ANOVA ----
# Two explanatory variables
# Usually the treatments, but in this case, using the pair variable.

lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)

anova(lsmodel2)
# Some of the DFs have been used by the new term
# Some of the error (SSE) has been used by SSR in the new term.

# F < 1 for pairing - so it implies a 'negative variance component'
# It has increased the relative proportion of SSE compared to SSR here
# When mean squares of the regression < the residuals it implies a problem with the experimental design
# Problems such as undersampling or pairs were not sampled randomly.


