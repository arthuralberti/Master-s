# Lista 1 - Arthur Alberti
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

## Parameters
setwd("/Users/arthuralberti/Desktop/programming/Econometria I/")

## Data Collecting
df <- read_dta("Econometrics Data/cps09mar/cps09mar.dta")

# Exercises
## 1
print(paste0("número? ", as.character(is.numeric(df$education)), 
             ", contínuo? ", as.character(is.integer(df$education))))
educ_range <- seq(min(df$education), max(df$education), by = 1)

print(paste0("número? ", as.character(is.numeric(df$age)), 
             ", contínuo? ", as.character(is.integer(df$age))))
age_range <- seq(min(df$age), max(df$age), by = 1)

### Regression
model <- lm(log(earnings) ~ age + I(age^2) + female + education, data = df) 
summary(model)
results <- tidy(model)
results$residuals <- NA
results$residuals[is.na(results$residuals)] <- "not robust"

### Robust Standard Errors Regression
coeftest(model, vcov = vcovHC(model, type = "HC0"))
results <- bind_rows(results, 
                     tidy(coeftest(model, vcov = vcovHC(model, type = "HC0"))))
results$residuals[is.na(results$residuals)] <- "HC0"

coeftest(model, vcov = vcovHC(model, type = "HC1"))
results <- bind_rows(results, 
                     tidy(coeftest(model, vcov = vcovHC(model, type = "HC1"))))
results$residuals[is.na(results$residuals)] <- "HC1"

coeftest(model, vcov = vcovHC(model, type = "HC2"))
results <- bind_rows(results, 
                     tidy(coeftest(model, vcov = vcovHC(model, type = "HC2"))))
results$residuals[is.na(results$residuals)] <- "HC2"

coeftest(model, vcov = vcovHC(model, type = "HC3"))
results <- bind_rows(results, 
                     tidy(coeftest(model, vcov = vcovHC(model, type = "HC3"))))
results$residuals[is.na(results$residuals)] <- "HC3"

coefficients <- coef(model)

print(paste0("the expected ln earning of a 20 year old woman is: ", 
             as.character(round(coefficients["(Intercept)"] + 
                                  coefficients["age"]*20 + 
                                  2*coefficients["I(age^2)"]*20 + 
                                       coefficients["female"], 
                                digits = 3)), " + ", 
             as.character(round(coefficients["education"], digits = 3)), 
             "*education"))

print(paste0("the average partial effect of another year of age on ln earnings is: ", 
             as.character(round(coefficients["age"], digits = 3)), " ",
             as.character(round(2*coefficients["I(age^2)"], digits = 3)),
             "*age."))

## 2
df$experience <- df$age - 15

### Regression
model2 <- lm(log(earnings) ~ age + I(age^2) + female + education + experience, 
             data = df) 
summary(model2)

print("as seen above, the model is ommiting the variable 'experience', 
      considering that it is created subtracting a scalar from another variable
      it must be a problem of multicolinearity")

cor(df$age, df$experience)

print("considering that age and experence have a correlation of 1, the software
      is ommiting the latter variable to keep the regression consistent.")

## 3
### Regression (homoskedastic)
model3 <- lm(log(earnings) ~ female + education + experience + I(experience^2), 
             data = df) 
summary(model3)
results3 <- tidy(model3)
results3$residuals <- NA
results3$residuals[is.na(results3$residuals)] <- "not robust"

### Robust (heteroskedastic) Standard Errors Regression
coeftest(model3, vcov = vcovHC(model3, type = "HC0"))
results3 <- bind_rows(results3, 
                     tidy(coeftest(model3, vcov = vcovHC(model3, type = "HC0"))))
results3$residuals[is.na(results3$residuals)] <- "HC0"

coeftest(model3, vcov = vcovHC(model3, type = "HC1"))
results3 <- bind_rows(results3, 
                     tidy(coeftest(model3, vcov = vcovHC(model3, type = "HC1"))))
results3$residuals[is.na(results3$residuals)] <- "HC1"

coeftest(model3, vcov = vcovHC(model3, type = "HC2"))
results3 <- bind_rows(results3, 
                     tidy(coeftest(model3, vcov = vcovHC(model3, type = "HC2"))))
results3$residuals[is.na(results3$residuals)] <- "HC2"

coeftest(model3, vcov = vcovHC(model3, type = "HC3"))
results3 <- bind_rows(results3, 
                     tidy(coeftest(model3, vcov = vcovHC(model3, type = "HC3"))))
results3$residuals[is.na(results3$residuals)] <- "HC3"

print("the parameters don't change, but the standard erros change marginally
      and consequentially the statistical significance of the parameters might
      change, in this specific case, even comparing different types of 
      heteroskedastic-robust standard errors the significance does not change 
      and the magnitude of the t-statistic vary around 10% for each parameter.
      considering that one can argue that there is not sufficient evidence of 
      heteroskedastic standard errors, then, it is better to assume 
      homoskedastic standard errors because they are more precise and gives 
      narrower confidence intervals, and potentially more power in our 
      hypothesis tests.")

bptest(model3)


clustered_errors <- round(diag(sqrt(abs(vcovCL(model3, cluster = df$region)))), 
                         digits = 3)

coefficients3 <- coef(model3)

print("considering that the number of unique regions used to cluster the 
      standard errors is extremely small, it might not be the best case to use
      this type of clustered errors, we might have to gamble with destiny and 
      not be able to account for within-cluster correlation.")

## 4
print("Frisch-Waugh-Love Theorem")
model4 <- lm(log(earnings) ~ age + I(age^2) + female, data=df)
resid4 <- residuals(model4)

model5 <- lm(education ~ age + I(age^2) + female, data = df)
resid5 <- residuals(model5)

model6 <- lm(resid4 ~ resid5, data = df)

summary(model6)
summary(model)

print("the coefficient is the same and the standard deviation is pratically the 
      same too.")

## 5
theta_hat = (coefficients3["education"]) / 
  (coefficients3["experience"] + 
     2*coefficients3["I(experience^2)"]*(18-3))

print(paste0("the ratio (theta) between returns of education and returns to ",
             "experience, given his age (18) is: ", 
             as.character(round(theta_hat, digits = 3))))

## 6
print("considering that we have θ = (β_educ) / (β_exp + 2*β_exp^2*exp), the",
      "gradiant ∇θ = [∂θ/∂β_educ, ∂θ/∂β_exp, ∂θ/∂β_exp^2] and the formula for the ",
      "asymptotic variance is: Var(θ_hat) ≈  ∇θᵀ * V * ∇θ ")

vcov(model)

sd_theta = 

## 7
print(paste0("the 90% confidence interval using theta_hat is: [",
             as.character(round(theta_hat - sd_theta*1.65, 
                                digits = 3)), ", ",
             as.character(round(theta_hat + sd_theta*1.65, 
                                digits = 3)), "]."))

# 8
model7 <- lm(log(earnings) ~ education + experience + I(experience^2) + female + female*education, data = df)
summary(model7)

waldtest(model, theta_hat = 1, )

