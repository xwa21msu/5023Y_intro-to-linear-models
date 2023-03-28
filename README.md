# 5023Y_intro-to-linear-models

## Workshop 1 30/01/23
### Introduction to Statistics
Today we set up the github repo, linked the scripts with the repo using the token and completed chapter 12 coursebook on BlackBoard. We were working with a new dataset: darwin, which looks at pairs of plants and their height when outcrossed or inbred. We cleaned the data, visualised it using a geom_point and geom_boxplot and then began making estimates on the means, sd, se and confidence intervals of the data.

## Workshop 2 10/02/23
### Introduction to Linear Models
Today we created a new script for workshop two and completed chapter 13 of the coursebook on Blackboard. We took the Darwin data, applied a linear model using base R and tidyverse functions, including `broom` that presents the results more clearly. We answered the hypothesis that selfed plants are smaller as a result of inbreeding with confidence intervals. We finally looked at the importance of making assumptions about the dataset, such as normal distribution, equal variance etc and explored ways to test this. We did not account for paired observations, but will do this next time.

## Workshop 3 17/02/23
### Testing
Today we created a new script exploring testing, t tests: paired and unpaired from chapter 14 of the coursebook. Sometimes adding in the paramter of 'paired' can add to the information to answer the hypothesis, and sometimes it doesn't, so it can be removed. We also looked at what type I and II errors are and how repeatability can help to improve confidence in the results of the test. A test statistic on its own can be misleading, so it is better to report estimates and confidence intervals which include uncertainty. 

## Workshop 4 24/02/23
### Regression
Today in another script, we explored regression; a linear model looking at the relationship of two continuous variables; wood density and hardness. We used Pearsons R to determine the association between the two variables, but used regression to determine if density causes hardness in wood. We looked at the equation y=a+bx+e and how to use this to predict unknown 'y' values. We also saw how to mean-center the data so that the regression line is not showing infinite values and how to use a standardised effect size. We finished up looking at the assumptions of the regression model and how this affects the confidence we have in the results.

## Workshop 5 03/03/23
### ANOVA
Today in a new script, we explored ANOVA: what it is, one and two tailed ANOVA and how to infer information from the output table it produced. There were numerous activities to complete in an R Markdown document where all the code was placed in chunks and the final plot and results were knit into the markdown file. There, we looked at assumptions of the model also to check if the model was a good fit - generally the data fit well apart from the extreme residuals that were not normally distributed.

## Workshop 6 10/03/23
### Multivariate Linear Models
In a new script, we looked at the effect of interactions between variables on a model which may change the mean effect more or less than that of the additive values of separate variables. We learned how to identify interactions on a graph by pinpointing non parallel lines and used drop1() to compare if the interaction term was having a large effect on explaining the variance in the data opposed to the individual variables. We finished up looking at how to report these and checking the fit of the model.

## Workshop 7 17/03/23
### Complex Linear Models
In a new script, we used fruitfly dataset to explore the hypothesis that increased sexual activity results in reduced lifespan in male fruitflies. The data was checked and a linear model was created, interaction terms were explored and the model simplified. Model fit was sure to be checked for goodness of fit and data transformation was explored also. The results were written up.

## Workshop 8 21/03/23
### General Linear Models
In this workshop we explored general linear models (GLM), in particular poisson regression for count data and logistic regression for binary data. For each GLM we used a separate dataset that would be approproate to apply those models to. We looked at interactions and overdispersion in terms of poisson regression to assess the fit of the model. For logistic regression, the change in coefficients to 'p' (probability) was explained as well as 'log odds' when working with binary data. Finally, predictions were made using the model creates (for logistic regression) and assumptions were broken down using 'performance' which adjusts its output appropriate to the model.
