# SET UP ----
## packages ----
library(tidyverse)
library(broom)

#_____________----

# EXPLORING STUDENTS T TEST ----

# base R
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)


# tidyverse
x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

# map_dfc combines values returned into a dataframe
t <- map_dfc(degf, ~dt(x, .x))
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="dashed")+
  theme_classic()

# critical t is defined by df
# when observed t (difference/SE) > critical t = statistically significant at alpha level


## values of critical t up to 30 df: ----
df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

## models ----
lsmodel1 <- lm(height ~ type, data = darwin)
summary(lsmodel1) # linear model with base R

broom::tidy(lsmodel1) # linear model with tidyverse

# test is done all rows of the table
# row two is of importance - tests null by comparing average observed difference between type
# estimate = average difference between type
# observed t value = probability of seeing this (at our sample size) if null is true

## observed value of t ----
tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]

#_____________----

# EXPLORING PAIRED T TESTS ----

# base R
lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin) # add in the factor for paired into linear model

# tidyverse
darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()

# now intercept is the height of crossed pair one.
# row two compares mean heights of crossed and selfed when in the same pairs
# row three-16 compares average difference of each pair (selfed and crossed) against pair 1
# only look at t value in row 2 for 'What is the difference in height between Cross and Self-pollinated plants when we hold pairs constant.'

## generating CIs ----
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows

# mean difference is the same as independent t test
# CIs different - increased uncertainty by adding in 'paired' parameter

m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()
# good t look at paired to see if it affects the explanation of height difference.
# if it doesn't, then it may be safe to remove it.

#_____________----

# POWER ----
# strength of a test to detect a difference when it is there.
# expressed as 1-B, where B = 20%
# so power must be 80% (chance of finding an effect if it is there)

#_____________----

# REPEATABILITY ----
set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)
# the new dataframe y contains the results of 20 new experiments to see if we can detect a typeI/II error

## tallying significant and insignificant results ----
y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())
# 6 non-significanr results
# 14 significant results
# therefore it is more likely the difference is significant
# literature may argue it is inconclusive

## considering CIs and estimates ----
y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()
# all show effect of inbreeding depression and have the same uncertainties
# just seeing the effects of sampling error

# much better way of reporting experiments




