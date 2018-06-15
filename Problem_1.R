## Homework 4, Problem 1

library(dplyr)

## Read the data
houseWork <-
  read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/houseWork.csv')
str(houseWork)

## a)
table(houseWork$sex)

## b)
muf <- mean(houseWork [houseWork$sex == 'f', "hours"])
mum <- mean(houseWork [houseWork$sex == 'm', "hours"])

## c)
houseworkbysex <-
  within(houseWork, {
    female <- ifelse (sex == 'f', 'TRUE', 'FALSE')
    male <- ifelse (sex == 'm', 'TRUE', 'FALSE')
  })

## d)
fit <- lm(hours ~ female , data = houseworkbysex)

## e)
summary(fit)
18.35829 - 32.81379
# The estimate of beta0 is 32.8138. It shows the population average house-work hours for men.
# The estimate of beta1 = -14.4555  is the difference between the population average house-work hours for women and men.

## f)
# H0: muf <= mum means muf - mum <= 0
# but we already know that beta1 =  muf - mum
# so H0: beta1 <= 0 vs. H1: beta1 > 0

## g)
beta1 <- -14.4555
n <- nrow(houseworkbysex)

testStatistic <- sqrt(n) * (beta1 / sd(houseworkbysex$hours))
criticalUpper <- qt(0.95, df = n - 1)
testStatistic > criticalUpper
pvalue <- (1 - pt(testStatistic, df = n - 1))
pvalue

## h)
# We cannot reject the null nypothesis
## at a 95% significance level (5% error probability)
## because the p-value is larger than 0.05. The p-value equals 1.

## i)
sd(houseworkbysex$hours)
mean(houseworkbysex$hours)
#the distributional assumptions of the test
#are that the observations are normally distributed.

## j)
fitAll <- lm(hours ~ female  + male , data = houseworkbysex)
summary(fitAll)