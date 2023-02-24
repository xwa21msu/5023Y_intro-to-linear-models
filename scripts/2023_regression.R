#_____________----

# PACKAGES----
library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)

#_____________----

# CLEAR ENVIRONMENT ----
rm(list = ls())

# IMPORT DATA ----
janka <- read_csv(here("data", "janka.csv"))

# DATA CLEANING ----
## check the data is in a tidy format ----

head(janka)
colnames(janka) # column names appropriate
janka <- janitor::clean_names(janka)
glimpse(janka)

## check for duplicated data ----

janka %>% 
  duplicated() %>% 
  sum() # sum of 0 = good

## check for typos by implausibly large values ----

janka %>% 
  summarise(min=min(dens, na.rm=TRUE), 
            max=max(dens, na.rm=TRUE))
janka %>% 
  summarise(min=min(hardness, na.rm=TRUE), 
            max=max(hardness, na.rm=TRUE))

## check for missing values ----
janka %>% 
  is.na() %>% 
  sum() #0 missing values

## summary ----
summary(janka)

#_____________----

# MAKE A PLOT ----
janka %>%
  ggplot(aes(x = dens, 
             y = hardness)) +
  geom_point()
# appears positively related and their correlation can be tested 

#_____________----

# PEARSONS CORRELATION ----
janka_pearson <- cor(janka$dens, janka$hardness, method = 'pearson')

# or:
# cor() does not have a data option so need to use the with() function
with(janka, cor(dens, hardness))

# or:
# with tidyverse (requires rstatix)
janka %>% 
  cor_test(dens, hardness)

#_____________----

# REGRESSION ----
# to test if density *causes* higher hardness

janka_ls1 <- lm(hardness ~ dens, data = janka) # left of tilde is response variable

## specify linear model method for line fitting ----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

## summary ----
summary(janka_ls1)

#_____________------

# MEAN CENTERED REGRESSION ----

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

#_____________----

# CONFIDENCE INTERVALS ----

# in base R:
confint(janka_ls1)

# in tidyverse:
broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)

# CIs do not span 0 -> significant relationship at alpha 0.05

#_____________----

# EFFECT SIZE ----
# standardised measure of how strong the relationship is
# R^2 = proportion of variation explained by linear regression analysis

# base r:
summary(janka_ls1)

# tidyverse:
janka_ls1 %>% 
  broom::glance()
# large effect size = 0.5, so this is of large effect

#_____________----

# ASSUMPTIONS ----

## calculating residuals of each datapoint
# base r;
predict(janka_ls1)
resid(janka_ls1)

# tidyverse:
janka_ls1 %>% 
  broom::augment() %>% 
  head()

## plotting the above data
augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red") # red line shows the residuals

# ideally we want to see:
# normal distribution of residuals (more near the mean)
# homogeneity (the average error is not greater at one end than the other)

# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")

p1+p2+p3

## making a function of the above code ----

model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

#_____________----

# NORMAL DISTRIBUTION ----

# base r:
plot(janka_ls1, which=c(2,2))

# tidyverse:
performance::check_model(janka_ls1, check=c("normality","qq"))

#_____________----

# EQUAL VARIANCE ----

# base r:
plot(janka_ls1, which=c(1,3))
# variance Y increases with X -> less confidence at higher values of X
# like the plot we did manually with raw vs fitted residuals
# this plot uses standardised residuals

# tidyverse:
performance::check_model(janka_ls1, check="homogeneity")

#_____________----

# OUTLIERS ----
# position 32 is a potential outlier.

# base r:
plot(janka_ls1, which=c(4,5))

# tidyverse:
performance::check_model(janka_ls1, check="outliers")

# cooks distance shows 32 might be having an outsized effect on the estimates

#_____________----

# PREDICTION ----
coef(janka_ls1)

# a + bx = y (to predict the hardness when wood density is 65)
-1160.49970 + 57.50667 * 65

# can automate this a bit:
coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65

# or use functions:
# base r:
predict(janka_ls1, newdata=list(dens=c(22,35,65)))

# tidyverse:
broom::augment(janka_ls1, 
               newdata=tibble(dens=c(22,35,65)))

#_____________----

# CONFIDENCE INTERVALS ----
# standard error ----

broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)

# 95% confidence intervals ----

broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

# list of predictions with CIs using emmeans ----
emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))
# uses a centered mean.

#_____________----

# ADDING PREDICTED VALUES TO EXISTING GRAPH ----

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))
  