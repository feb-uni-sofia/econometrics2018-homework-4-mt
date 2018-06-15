## Homework 4, Problem 2
crime <-
  read.delim(
    'https://s3.eu-central-1.amazonaws.com/econometrics2018/data/crime.csv',
    stringsAsFactors = FALSE
  )
str(crime)
#install.packages(c('ggplot2', 'dplyr'))

library(dplyr)
library(ggplot2)

## a)
fit <- lm(C ~ HS , data = crime)
summary(fit)
##Beta1 is 1.4860, which is >0.
#This means that when HS increases C increases as well.
#Higher percent of people completing high school is connected with higher crime rate.

## b)
ggplot(data = crime, aes(x = HS, y = C)) +
  geom_point() +
  geom_abline(slope = 1.4860, intercept = -50.8569)

ggplot(data = crime, aes(x = U, y = C)) +
  geom_point()

ggplot(data = crime, aes(x = I, y = C)) +
  geom_point()

## c)
fit2 <- lm(C ~ HS + U, data = crime)
summary(fit2)
#Beta1 is -0.5834, which is negative.
#When the percentage of people completing high school increases
#the crime rate decreases.

#Beta 2 is  0.6825, which is positive.
#This means that when the percentage of people living in urban areas increases
#the crime rate also increases.


## d)
fitHS_U <- lm(HS ~ U , data = crime)
summary(fitHS_U)
#we can notice that there is a positive connection between urbanization and people finishing high school.
#The percantage of people with HS degree is higher in urban areas.

## e)
fit3 <- lm(C ~ HS + U + I, data = crime)
summary(fit3)

var.test(
  lm(C ~ HS , data = crime),
  lm(C ~ I , data = crime) ,
  ratio = 1,
  alternative = c("two.sided"),
  conf.level = 0.95
)

#P-value is greater than 0.05
#so we cannot reject the null hypothesis at a 5% significance level.
#H0 means:
#1)there is no connection between people finishing high school and the crime rate
#2)there is no connection between the median income in the county and the crime rate
