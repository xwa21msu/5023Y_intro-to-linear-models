library(tidyverse)
library(here)
library(kableExtra)

darwin <- read_csv(here("data", "darwin.csv"))

# CHECK DATA----

#check data is in a tidy format
head(darwin)

colnames(darwin) #column names look good

#clean up column names
darwin <- janitor::clean_names(darwin)

glimpse(darwin)

# check for duplicate rows in the data
darwin %>% 
  duplicated() %>% 
  sum() #sum of duplicated rows is 0

#check for typos by searching for implausibly large values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE)) #no very large or very small values

#look for typos in character variables
darwin %>% 
  distinct(type) #no typos

darwin %>%
  distinct(pair)

#check for missing values
darwin %>% 
  is.na() %>% 
  sum() #0 missing values

#summary
summary(darwin)

#__________________________----

#DATA VISUALISATION----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

darwin %>%
  ggplot(aes(x=type,
             y=height))+
  geom_boxplot()

#determining the mean and standard deviation----
darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

#plot of mean and sd of the two types----

# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

#use kable extra functions to make a nice table ----
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

#______________----

#ESTIMATION----
#of differences between each pair of plants

# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights ----

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

#calculate the mean difference between paired plants and the variance (sd) ----
difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

#standard error of the difference ----
difference_summary %>% 
  mutate(se= sd/sqrt(n))
# the average difference in height was 2.62 ± 1.22 inches (mean ± SE).

#standard error is a measure of uncertainty of the estimates.
#larger sample size = lower se = higher confidence

#______________----

#UNCERTAINTY ----
#working out the confidence in the differences in the population means

#Null hypothesis - there is no difference in the mean height of self vs crossed plants
#Alternate hypothesis - inbreeding reduces the fitness of the selfed plants, observed as selfed plants on average being smaller than crossed plants

#normal distribution ----
#idealised normal distribution:

#create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

#confidence intevals ----
#because ± 2 standard errors covers the central 95% of the area under the normal curve, we refer to this as our 95% confidence interval.
#can have many confidence intevals
#commonly we refer to standard error (68% CI), 95% and 99%

#calculating 95% confidence intevals
lowerCI <- 2.62-(2*1.22) #1.22 is the SE

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

#says we are confident we would capture the true mean in 95% of our experiments.


