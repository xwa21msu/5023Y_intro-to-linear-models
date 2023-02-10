library(tidyverse)
library(GGally) # produce a graph of estimated mean difference with approx 95% CIs.
library(emmeans)
library(performance)
library(broom)
install.packages("broom.helpers")

# BASICS OF LINEAR MODELS ----

lsmodel0 <- lm(formula = height ~ 1, data = darwin) 
# 1 as an explanatory variable means mean is just calculated for all plants.

summary(lsmodel0)
broom::tidy(lsmodel0) #a cleaner way of visualising the results.
# estimate is the estimate of model coefficient, in this case the mean.


# COMPARING MEANS ----
# e.g. analysing difference in plant height as a function of pollination:

lsmodel1 <- lm(height ~ type, data=darwin)
# note that the following is identical
# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

broom::tidy(lsmodel1)
# intercept = mean height of crossed plants = 20.2
# typeself = difference in mean height between crossed and selfed = 2.62 shorter than crossed.

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height)) # to check if the linear model was correct.

summary(lsmodel1) # look at a fuller summary for more info.

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw() # model information on graph

# STANDARD ERROR OF DIFFERENCE ----
# uses pooled variance, so assumes equal variance across two groups - need to check this later


# CONFIDENCE INTERVALS ----

confint(lsmodel1)
broom::tidy(lsmodel1, conf.int=T)
# CIs for mean height of crossed plants and CIs for the mean difference in height
# lower and upper bounds are 2.5% and 97.5% for T-distribution.

# ANSWERING THE QUESTION ----
# without using the P-values, only the CIs.

# hypothesis: Darwin's original hypothesis was that self-pollination would reduce fitness
# null: no effect of pollination type, and therefore no difference in the average heights
# i.e. does a difference of 0 lie in the 95% confidence intervals?
# if CI doesn't contain the intercept value, can reject the null

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.99)
# with greater confidence intervals (0.99), the null could be accepted.

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)


# CALCULATING THE OTHER TREATMENT MEAN AND SE ----

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

means <- emmeans::emmeans(lsmodel1, specs = ~ type)
means # another way to do this using emmeans package, showing all levels

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL)) # can then present emmeans info in a graph

# because of pooled variance, the onyl difference in SE should be if the sample sizes are different.


# ASSUMPTION CHECKING ----
# assumption1: that the residual/unexplained variance in our data is approximately normally distributed.
# assumption2: that the residual/unexplained variance is approximately equal between our groups.

# Residuals are the differences between the observed values and the fitted values produced by the model - in this case the heights of the plants against the treatment means.

performance::check_model(lsmodel1)
plot(lsmodel1)

performance::check_model(lsmodel1, check=c("normality","qq")) # check for normality

plot(lsmodel1, which=c(2,2))
# data on Y, theoretical normal on X, should follow each other.
# we have some outliers - not normal at the extremes

performance::check_model(lsmodel1, check="homogeneity") # check for equal variance
plot(lsmodel1, which=c(1,3))

# outliers ----

performance::check_model(lsmodel1, check="outliers")

plot(lsmodel1, which=c(4,4))
# cooks distance = how much 'leverage' a single data point is exerting on the model, if it is too high, it may be having an outsized effect on the estimates.

# SUMMARY ----
# not perfect, and not assumed paired observations
# basically carried out a students t-test, not a paired one.

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)
