# Lista 2 - Arthur Alberti

## Cleaning Workspace
rm(list = ls())

## Libraries
library(haven)
library(kableExtra)
library(knitr)
library(tidyverse)
library(sandwich)
library(lmtest)
library(ggplot2) 
library(dplyr)
library(broom)
library(Deriv)
library(estimatr)
library(dagitty)
library(AER)

## Parameters
setwd("/Users/arthuralberti/Desktop/programming/Econometria I/")
par(family = "Times New Roman")

## Data Collecting
df <- read_dta("Econometrics Data/AK1991/AK1991.dta")

# Exercises
## 1
### Filtering the dataframe
colnames(df)

df_b <- df[df$black == 1, ]

unique(df_b$black)

### Regression
model <- lm(logwage ~ edu + married + smsa + factor(yob) + factor(state) 
            + factor(region), data = df_b)
summary(model)
results <- tidy(model)
results$residuals <- NA

## 2
dag <- dagitty('dag {
 "Education" -> "Wage"
 "Parents Wealth" -> "Education"
 "Parents Wealth" -> "Wage"
}')

plot(dag)

print("The OLS estimates might not be trust-worthy because there could be a 
      potencial source of endogeneity in the regressors, there could be 
      non-observable variables highly correlated with both education and wages,
      which would bias the estimates.")

## 3
### First Stage Regression
model2 <- lm(edu ~ factor(yob*qob) + married + smsa + factor(yob) + factor(state)
               + factor(region), data = df_b)
summary(model2)

### Reduced Form Regression
summary(model)

### 2SLS Regression
model5 <- ivreg(logwage ~ edu + smsa + married + factor(yob) + factor(state) 
                + factor(region) | factor(yob*qob) + smsa + married +  
                + factor(yob) + factor(state) + factor(region), data = df_b)
summary(model5)

## 4
print("Considering that the 2SLS estimates are the more precise ones, this could
      be used as evidence that the OLS has a positive bias on the estimates of
      education, overestimating the causal effect of education on wages. This could
      be becasue of unobservables that are highly correlated with both wages and
      education .")

## 5
### Endogeneity Tests and Overidentification Tests
summary(object = model5, diagnostics = TRUE)

print("Considering the Wu-Hausman Test, we can conclude that both the IV and OLS
      estimates are consistent. This means that education should be an exogenous
      variable. But could also mean that we have a weak instrument. Although, the 
      Sargan Test shows that we are using a valid instrument, even the weak test 
      conffirms that it is a weak instrument.")

## 6
edu_hat <- fitted(model2)

model6 <- lm(logwage ~ edu_hat + married + smsa + factor(yob) + factor(state) 
             + factor(region), data = df_b)
summary(model6)

## 7
model2_res <- model2$residuals

#Second stage: regress the structural model with the residuals from the first stage
model7 <- lm(logwage ~ edu + married + smsa + factor(yob) + factor(state) 
             + factor(region) + model2_res, data = df_b)
coeftest(model7, vcov = vcovHC)

## 8
summary(object = model5, diagnostics = TRUE)

#   As shown by the 2SLS estimation in Exercise 3 the null hypothesis for the 
# Weak Instrument test is not rejected, so the instrument selected is weak i.e. 
# weakly correlated with education. This aligns with the fact that the test for 
# endogeneity also does not reject the null hypothesis. Overall, while the 
# instrument is valid and fully identified (as per definition 12.1 in Hansen, 2023) 
# its not a good instrument for our variable of interest, and in a research paper 
# another IV would have to be tested.

## 9
model8 <- ivreg(logwage ~ edu + married + smsa + factor(yob) + factor(state) 
                + factor(region) | married + smsa + factor(qob) + factor(yob) 
                + factor(state) + factor(region), data = df_b)
summary(object = model8, diagnostics = TRUE)

print("Using only quarter-of-birth as an instrument the null hypothesis of the 
      weak instrument test is rejected, which means that qob is sufficiently 
      correlated with education. However the endogeneity test still does not
      reject the null hypothesis, meaning that the OLS and 2SLS estimators are 
      consistent and should be the similar. So, in my own research if there is 
      no reason to use an instrument other than endogeneity, I'd use the OLS 
      estimators because they are more efficient than the 2SLS.")

## 10
print("given the required assumptions to have a valid instrument, it helps us
      estimate the Local Average Treatment Effect, that represents the average
      treatment effect for the compliers. This affects the interpretation of the
      results by disconsidering the effect of the always takers, so it might not 
      be generalizable to the entire population. It provides a better causal path
      than a regression with endogeneity, but it has to be used with caution.")

## 11

print("Controlling for region-of-residence and urban information could arise a bad control 
      problem. If these two variables are highly correlated and have a both of them  
      have a high Beta, they could be diminishing the parameter that you're trying 
      to estimate. By controlling for both variables we could be killing this causal 
      path and getting biased estimates. Controlling for one of them might be enough.")
