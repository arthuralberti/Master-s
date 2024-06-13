# Lista 3 - Arthur Alberti

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
library(plm)
library(fixest)
library(tidysynth)

## Parameters
setwd("/Users/arthuralberti/Desktop/programming/Econometria I/")
par(family = "Times New Roman")

## Data Collecting
df <- read_dta("Econometrics Data/DS2004/DS2004.dta")

# Exercises
## 1
### Cleaning the dataframe
colnames(df)

unique(df$sameblock)
unique(df$month)

df <- df %>%
  mutate(post = ifelse(month > 06, 1, 0))

### Regression
df %>%
  group_by(sameblock, post) %>%
  summarize(mean_value = mean(thefts),
            sd_value = sd(thefts))

df_2by2_mean <- df %>%
  group_by(sameblock, post) %>%
  summarize(mean_value = mean(thefts)) %>%
  pivot_wider(names_from = sameblock, values_from = c(mean_value)) %>%
  rename(control = "0", sameblock = "1") %>%
  mutate(diff = sameblock - control) %>%
  select(-post)

df_2by2_mean <- rbind(df_2by2_mean, df_2by2_mean[2, ] - df_2by2_mean[1, ])
df_2by2_mean <- round(df_2by2_mean, digits = 4)
df_2by2_mean <- as.data.frame(df_2by2_mean)
row.names(df_2by2_mean) <- c("pre", "post", "diff")

kable(df_2by2_mean, format = "markdown", booktabs = TRUE, row.names = TRUE,
      caption = "Mean in the 2-by-2 Diff-in-Diff") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  column_spec(1, bold = TRUE)

model <- lm(thefts ~ post * sameblock, data = df)

sd <- vcovHC(model) %>% 
  diag() %>% 
  sqrt()
sd <- round(sd, digits = 4)

kable(sd, format = "markdown", booktabs = TRUE, row.names = TRUE,
      caption = "Standard Deviation in the 2-by-2 Diff-in-Diff") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  column_spec(1, bold = TRUE)

print("Considering the results of the table, we can know that the difference in 
      thefts between control and treatment group before the treatment is 0.017
      while after treatment the difference is -0.061, and the difference between
      treatment and control and pre and post treatment is -0.07886 (considering
      the 'rule-of-thumb', dividing the parameter by it's standard deviation we 
      have that it is significant considering alpha = 0.05), indicating 
      that the having a jewish institution on the block had an effect on thefts
      by allocating police on these blocks.")

## 2
colnames(df)

# Canonical Differences-in-Differences
model <- lm(thefts ~ post * sameblock, data = df)
coefs <- coef(model)

# 2-Way Fixed Effects Differences-in-Differences
model <- lm(thefts ~ post * sameblock + factor(block) + factor(month), data = df)
coefs2 <- coef(model)

coefs[c("post", "sameblock", "post:sameblock")]
coefs2[c("post", "sameblock", "post:sameblock")]

print("Note that in both cases the differences-in-differences coefficients are 
      the same, this happens because they are analogous, the dummies of individuals
      and time works as fixed effects that are invariable over time and invariable
      over individuals, respectively.")

## 3
colnames(df)

# Canonical Differences-in-Differences
model <- lm(thefts ~ post * sameblock + bank, data = df)
coefs3 <- coef(model)

model <- lm(thefts ~ post * sameblock + gasstation, data = df)
coefs4 <- coef(model)

model <- lm(thefts ~ post * sameblock + public, data = df)
coefs5 <- coef(model)

model <- lm(thefts ~ post * sameblock + bank + gasstation + public, data = df)
coefs6 <- coef(model)

# 2-Way Fixed Effects Differences-in-Differences
df$id <- 1:nrow(df)

model <- lm(thefts ~ post * sameblock + factor(block) + factor(month) 
             + bank + gasstation + public, data = df)
coefs7 <- coef(model)

coefs3[c("post", "sameblock", "post:sameblock")]
coefs4[c("post", "sameblock", "post:sameblock")]
coefs5[c("post", "sameblock", "post:sameblock")]
coefs6[c("post", "sameblock", "post:sameblock")]
coefs7[c("post", "sameblock", "post:sameblock")]

print("Considering the control options in the database, the most plausible one
      would be 'bank', it would be expected to have a negative impact in crimes
      and if not controlled for, there would be confounding bias. But considering
      that we are estimating a differences-in-differences, when controlling for
      fixed effects, all of the control variavles that do not vary over time are
      'excluded' with the fixed effects. So adding controls does not affect the
      estimates of the model.")

## 4
# Canonical Differences-in-Differences
model <- lm(thefts ~ post * sameblock, data = df)
coefs8 <- coef(coeftest(model, vcov = vcovHC(model, type = "HC0")))
coefs8[c("post", "sameblock", "post:sameblock")]

sd <- sd(model$residuals)
se <- sqrt(diag(vcovHC(model)))

print(sd)
se[c("post", "sameblock", "post:sameblock")]

# 2-Way Fixed Effects Differences-in-Differences - Spherical Errors
model <- feols(thefts ~ post * sameblock | block + month, 
                   data = df,
                   vcov = "iid")$se
print(model)

### 2-Way Fixed Effects Differences-in-Differences - Heteroskedastic Errors
model <- feols(thefts ~ post * sameblock | block + month, 
                     data = df,
                     vcov = "hetero")$se
print(model)

### 2-Way Fixed Effects Differences-in-Differences - Autocorrelated Errors
model <- feols(thefts ~ post * sameblock | block + month, 
                   panel.id = c("block", "month"),
                   data = df,
                   vcov = "NW")$se
print(model)

# 2-Way Fixed Effects Differences-in-Differences - Robust Standard Errors
model <- lm(thefts ~ post * sameblock + factor(block) + factor(month), data = df)

coefs9 <- coef(coeftest(model, vcov = vcovHC(model, type = "HC0")))
coefs9[c("post", "sameblock", "post:sameblock")]

sd2 <- sd(model$residuals)
se2 <- sqrt(diag(vcovHC(model)))

print(sd2)
se2[c("post", "sameblock", "post:sameblock")]

print("Considering that the estimated standard errors are very similar comparing
      different assumptions for the errors, we'd still consider using the 
      Newey-West estimator, which generates robust standard error allowing for 
      autocorrelation and heteroskedasticy.")

## 5
# Differences in Differences
plot_data <- df %>%
  group_by(sameblock, month) %>%
  summarise(mean_outcome = mean(thefts))

ggplot(plot_data, aes(x = month, y = mean_outcome, group = sameblock, color = sameblock)) +
  geom_line() +
  geom_point() +
  labs(x = "Months", y = "Average Thefts", title = "Differences-in-Differences") + 
  geom_vline(xintercept = 7, linetype = "dashed") +
  theme_minimal() 

# Dynamic Differences-in-Differences / Event-Study / Staggered Differences-in-Differences
model = feols(thefts ~ (sameblock):i(month,7) + sameblock + month, 
              cluster = ~block, data = df)
summary(model)

iplot(model, 
      xlab = 'Months',
      main = 'Dinamyc Differences-in-Differences')

print("Considering the pre-treatment part, it seems that the the difference between
      the control and treatment group trends were not constant, they were close, 
      but it doesn't seem close enough to argue in favor of parallel trends. And
      for the dynamic differences-in-differences, it seems that the after treatment
      the coefficients were not statistically signifficant, so, we don't have enough
      evidence to conlcude that the parameters are not different from zero after
      the treatment. Other than that, the coefficients pre-treatment were
      statistically signifficant, suggesting that the trends were not parallel before
      treatment.")

## 6
print("Considering the dynamic differences-in-differences graph plotted in the last
      questions, we have enough evidence to suggest that the impact of policing
      is short-lived if it has any. We can't conclude that there is an effect.")

## 7
colnames(df)

# Canonical Differences-in-Differences
model <- lm(thefts ~ post * oneblock, data = df)
coefs10 <- coef(coeftest(model, vcov = vcovHC(model, type = "HC0")))

# 2-Way Fixed Effects Differences-in-Differences
model <- lm(thefts ~ post * oneblock + factor(block) + factor(month), data = df)
coefs11 <- coef(model)

coefs10[c("post", "oneblock", "post:oneblock")]
coefs11[c("post", "oneblock", "post:oneblock")]

# Differences in Differences
plot_data <- df %>%
  group_by(oneblock, month) %>%
  summarise(mean_outcome = mean(thefts))

ggplot(plot_data, aes(x = month, y = mean_outcome, group = oneblock, color = oneblock)) +
  geom_line() +
  geom_point() +
  labs(x = "Months", y = "Average Thefts", title = "Differences-in-Differences") + 
  geom_vline(xintercept = 7, linetype = "dashed") +
  theme_minimal() 

# Dynamic Differences-in-Differences / Event-Study / Staggered Differences-in-Differences
model = feols(thefts ~ (oneblock):i(month,7) + oneblock + month, 
              cluster = ~block, data = df)
summary(model)

iplot(model, 
      xlab = 'Months',
      main = 'Dinamyc Differences-in-Differences')

print("Considering the canonical differences-in-differences graph, pre-treatment the
      trends seems to be parallel, but looking at the statistically signifficant
      parameters for the interaction between treatment and the months before it, 
      there is evidence that the trends were not parallel. And for post-treatment,
      for all of the months we can't reject the null hypothesis that the paramters
      of the interactions between months are treatment are not different from zero.
      Considering the dynamic differences-in-differences, the graph is clearer, 
      in the y axis is the coefficient and it's standard deviation, so we can see 
      that there is not enough evidence to conclude that treatment had an effect 
      on thefts.")

## 8 and 9
print("I left this question in '#' because it takes too much time to run, you can
      run the entire code and check that it works perfectly and later check only 
      this question by deleting the '#'s.")

# synthetic_block <- df %>% 
#   synthetic_control(outcome = thefts,
#                     unit = block,
#                     time = month,
#                     i_unit = 797,
#                     i_time = 6,
#                     generate_placebos=T
#   ) %>% 
#   generate_predictor(time_window = 4:12,
#                      sameblock = mean(sameblock),
#                      public = mean(public),
#                      bank = mean(bank),
#                      gasstation = mean(gasstation),
#                      distance = mean(distance)) %>%
#   generate_weights(optimization_window = 4:12,
#                    margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6
#   ) %>%
#   generate_control()
# 
# synthetic_block %>%
#   plot_trends() %>% 
#   plot_differences() %>% 
#   plot_weights()

print("It takes too much time and computational power to run this synthetic
      control procedure, to run it a 100 times would be too costly. But the idea
      would be to match the description of a block with characteristics of others
      and create synthtically a similar one, we could be able to see if it fits
      analyzing the graphs.")

## 10 and 11
colnames(df)

pdata <- pdata.frame(df, index = c("block", "month"))

# Pooled OLS Regression Differences-in-Differences
model <- plm(thefts ~ post * sameblock + factor(block) + factor(month), 
             data = pdata, model = "pooling")
summary(model)
coefs12 <- coef(model)
coefs12[c("post", "sameblock", "post:sameblock")]

# Within: Pooled OLS Regression Differences-in-Differences with Time-Specific Errors
model <- plm(thefts ~ post * sameblock + factor(block) + factor(month), 
             data = pdata, model = "within")
summary(model)
coefs13 <- coef(model)
coefs13[c("post", "sameblock", "post:sameblock")]

print("Considering that the effect is the same, it suggests that the treatment
      effect may not be affected by endogeneity. Because running a Pooled OLS
      regression you assume that there is no correlation between the explanatory
      variables and the error term, so, you should expect to see differences in 
      the coefficients of the pooled OLS and fixed effect models.")

## 12
# Random Effects: Pooled OLS Regression Differences-in-Differences
model <- plm(thefts ~ post * sameblock, 
             data = pdata, model = "random")
summary(model)
coefs14 <- coef(model)
coefs14[c("post", "sameblock", "post:sameblock")]

print("Considering the last 3 models used, the Pooling, the Within and the Random
      Effects, we can see that the estimated parameter stays the saem across them, 
      showing evidence that endogeneity of the treatment might not be something
      we have to worry. And for the variance, we can look at each standard deviation
      and the statisic significance of the parameter in each model, but overall, it
      stays statistically signifficant at alpha = 0.05 for all three models.")

## 13
fe_model <- plm(thefts ~ post * sameblock + factor(block) + factor(month), 
             data = pdata, model = "within")

re_model <- plm(thefts ~ post * sameblock, 
             data = pdata, model = "random")

hausman <- phtest(fe_model, re_model)
print(hausman)

print("Considering the p-value of 1, there is no evidence to reject the null hypothesis, 
      which suggests that the coefficients estimated in the model are consistent. 
      Therefore, the model appears to be reliable in terms of the estimated 
      coefficients, indicating that there is no significant difference between the
      coefficients estimated from two different models. In this case, we should
      resort to theoretical considerations and model fit to choose which model
      to use.")

## 14
df$placebo <- sample(c(0, 1), nrow(df), replace = TRUE)

# Placebo Treatment
df_control_p <- df[df$placebo == 1, ]

ggplot(df_control_p, aes(x = thefts)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
  labs(x = "Thefts", y = "Frequency", 
       title = "Histogram of Thefts in the Placebo Control Group") +
  theme_minimal()

df_treatment_p <- df[df$placebo == 0, ]

ggplot(df_control_p, aes(x = thefts)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
  labs(x = "Thefts", y = "Frequency", 
       title = "Histogram of Thefts in the Placebo Treatment Group") +
  theme_minimal()

# Actual Treatment
df_control_a <- df[df$sameblock == 1, ]

ggplot(df_control_a, aes(x = thefts)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
  labs(x = "Thefts", y = "Frequency", 
       title = "Histogram of Thefts in the Actual Control Group") +
  theme_minimal()

df_treatment_a <- df[df$sameblock == 0, ]

ggplot(df_control_a, aes(x = thefts)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
  labs(x = "Thefts", y = "Frequency", 
       title = "Histogram of Thefts in the Actual Treatment Group") +
  theme_minimal()

probability <- mean(df$placebo >= df$sameblock)

## 15
u <- feols(thefts ~ 1 | month + block, data = df)

df$police <- 10*round(u$residuals*20) + 50*df$sameblock*df$post + 70*df$sameblock 
+ 100*df$public + 100*df$bank + 50*df$gasstation + 100*(df$barrio == "Once") +
  sample(100:500, size = 7884, replace = TRUE)

colnames(df)
pdata <- pdata.frame(df, index = c("block", "month"))

# 2-Way Fixed Effects: using Sameblock
model <- plm(thefts ~ post * sameblock + factor(block) + factor(month), 
             data = pdata, model = "within")
summary(model)
coefs15 <- coef(model)

# 2-Way Fixed Effects: using Police
model <- plm(thefts ~ post * police + factor(block) + factor(month), 
             data = pdata, model = "within")
summary(model)
coefs16 <- coef(model)

# 2-Way Fixed Effects: with Police and Instruments
model <- plm(thefts ~ police + factor(block) + factor(month) | 
               sameblock * post + factor(block) + factor(month), 
             data = pdata, model = "within")
summary(model)
coefs17 <- coef(model)

coefs15[c("post", "post:sameblock")]
coefs16[c("post", "police", "post:police")]
coefs17[c("police")]

print("Comparing the 3 options, the sameblock as the treatment group, the police 
      as the treatment group and the sameblock as instrument for the police. We have
      that the 'real' impact of police as instrumented by sameblock is much lower
      than estimated without considering the endogeneity impacts. After considering
      this and allowing for it in the model, the impact is much lower, it goes from
      -0.078 to -0.002, but it is still signifficant for alpha = 0.05. The signal
      of the impact is negative in all case, meaning that the impact of adding more
      policing in certain blocks diminishes thefts.")

