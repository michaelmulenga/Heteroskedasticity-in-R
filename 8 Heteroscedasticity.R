# Heteroscedasticity
  
# Outline:
#   Graphical analysis of heteroscedasticity 
#   Heteroscedasticity tests
#   Breusch-Pagan test
#   White test
#   Alternative White test
#   Heteroscedasticity robust standard errors
#   Weighted Least Squares (WLS)
#   Feasible Generalized Least Squares (FGLS)

# Data files: 
#   hprice1.csv

# setup
rm(list = ls()) 
directory <- "C:/Users/MBM/Documents/Heteroskedasticity-in-R"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Graphical analysis of heteroscedasticity --------------------------------

# Data set on house prices
#hprice1 <- read.csv(paste0(directory, "hprice1.csv"))
hprice1 <- read.csv("C:/Users/MBM/Documents/Heteroskedasticity-in-R/hprice1.csv")
hprice1 %>% 
  select(price, lprice, lotsize, sqrft, bdrms) %>% 
  str

hprice1 %>% 
  select(price, lprice, lotsize, sqrft, bdrms) %>% 
  stargazer(type = "text")

hprice1 %>% 
  select(price, lprice, lotsize, sqrft, bdrms) %>%
  head(10)

# Regression model for price
model_0 <- lm(price ~ lotsize + sqrft + bdrms, hprice1)
summary(model_0)
hprice1 %<>% mutate(uhat = resid(model_0))

# Graph of residuals against independent variable
ggplot(data = hprice1, mapping = aes(x = sqrft, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Residuals', x = 'Square feet, sqrft')

# Graph of residuals against fitted values
hprice1 %<>% mutate(yhat = fitted(model_0))
ggplot(data = hprice1, mapping = aes(x = yhat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Residuals', x = 'Fitted values')

# Regression model for lprice
model_1 <- lm(lprice ~ llotsize + lsqrft + bdrms, hprice1)
summary(model_1)
hprice1 %<>% mutate(uhat1 = resid(model_1))

# Graph of residuals against independent variable
ggplot(hprice1) + 
  theme_bw() + 
  geom_point(aes(x = lsqrft, y = uhat1)) +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Residuals', x = 'Log square feet, lsqrft')

# Graph of residuals against fitted values
hprice1 %<>% mutate(yhat1 = fitted(model_1))
ggplot(data = hprice1, mapping = aes(x = yhat1, y = uhat1)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Residuals', x = 'Fitted values')


# Heteroscedasticity tests ------------------------------------------------
  
# Heteroscedasticity tests involve estimating the regression model, 
# regressing the squared residuals uhatsq on combination of independent variables
# and doing F-test and LM-test for joint coefficient significance.

# Generate squares and interaction of independent variables
hprice1 %<>% mutate(lotsizesq = lotsize^2,
                    sqrftsq = sqrft^2,
                    bdrmssq = bdrms^2,
                    lotsizeXsqrft = lotsize*sqrft,
                    lotsizeXbdrms = lotsize*bdrms,
                    sqrftXbdrms = sqrft*bdrms)


# Heteroscedasticity tests for price --------------------------------------

# Regression model is the same as model_0
# model_0 <- lm(price ~ lotsize + sqrft + bdrms, hprice1)
summary(model_0)

# Get residuals(uhat) and predicted values(yhat), and square them
hprice1 %<>% mutate(uhatsq = uhat^2,
                    yhatsq = yhat^2)

# Breusch-Pagan test ------------------------------------------------------

# Regression for Breusch-Pagan test
model_BP <- lm(uhatsq ~ lotsize + sqrft + bdrms, hprice1)
summary(model_BP)

# Number of independent variables k1 
(k1 <- model_BP$rank - 1)

# F-test and LM-test for heteroscedasticity
(r2 <- summary(model_BP)$r.squared) # R-squared
(n <- nobs(model_BP)) # number of observations

( F_stat <- (r2/k1) / ((1-r2)/(n-k1-1)) ) # F-statistic
( F_pval <- pf(F_stat, k1, n-k1-1, lower.tail = FALSE) ) # p-value

( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k1, lower.tail = FALSE)) # p-value

# White test --------------------------------------------------------------
  
# Regression for White test 
model_White <- lm(uhatsq ~ lotsize + sqrft + bdrms + lotsizesq + sqrftsq +
bdrmssq + lotsizeXsqrft + lotsizeXbdrms + sqrftXbdrms, hprice1)
summary(model_White)

# Number of independent variables k2
(k2 <- model_White$rank - 1)

# F-test and LM-test for heteroscedasticity
(r2 <- summary(model_White)$r.squared) # R-squared
(n <- nobs(model_White)) # number of observations

( F_stat <- (r2/k2) / ((1-r2)/(n-k2-1)) ) # F-statistic
( F_pval <- pf(F_stat, k2, n-k2-1, lower.tail = FALSE) ) # p-value

( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k2, lower.tail = FALSE)) # p-value

# Alternative White test --------------------------------------------------

# Regression for alternative White test
model_Alt <- lm(uhatsq ~ yhat + yhatsq, hprice1)
summary(model_Alt)

# Number of independent variables k3
(k3 <- model_Alt$rank - 1)

# F-test and LM-test for heteroscedasticity
(r2 <- summary(model_Alt)$r.squared) # R-squared
(n <- nobs(model_Alt)) # number of observations

( F_stat <- (r2/k3) / ((1-r2)/(n-k3-1)) ) # F-statistic
( F_pval <- pf(F_stat, k3, n-k3-1, lower.tail = FALSE) ) # p-value

( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k3, lower.tail = FALSE)) # p-value

# All tests show heteroscedasticity for price 

# Heteroscedasticity tests for log price ----------------------------------

# Regression model for log price
model_2 <- lm(lprice ~ lotsize + sqrft + bdrms, hprice1)
summary(model_2)

# Get residuals and predicted values, and square them
hprice1 %<>% mutate(uhat1 = resid(model_2),
                    yhat1 = fitted(model_2), 
                    uhat1sq = uhat1^2, 
                    yhat1sq = yhat1^2)

# Breusch-Pagan test ----
  
# Regression for Breusch-Pagan test
model_BP <- lm(uhat1sq ~ lotsize + sqrft + bdrms, hprice1)
summary(model_BP)

# Number of independent variables k1
(k1 <- model_BP$rank - 1)

# F-test and LM-test for heteroscedasticity
(r2 <- summary(model_BP)$r.squared) # R-squared
(n <- nobs(model_BP)) # number of observations

( F_stat <- (r2/k1) / ((1-r2)/(n-k1-1)) ) # F-statistic
( F_pval <- pf(F_stat, k1, n-k1-1, lower.tail = FALSE) ) # p-value

( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k1, lower.tail = FALSE)) # p-value

# White test ----

# Regression for White test

model_White <- lm(uhat1sq ~ lotsize + sqrft + bdrms + lotsizesq + sqrftsq + 
                    bdrmssq + lotsizeXsqrft + lotsizeXbdrms + sqrftXbdrms, 
                  hprice1)
summary(model_White)

# Number of independent variables k2
(k2 <- model_White$rank - 1)

# F-test and LM-test for heteroscedasticity
(r2 <- summary(model_White)$r.squared) # R-squared
(n <- nobs(model_White)) # number of observations

( F_stat <- (r2/k2) / ((1-r2)/(n-k2-1)) ) # F-statistic
( F_pval <- pf(F_stat, k2, n-k2-1, lower.tail = FALSE) ) # p-value

( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k2, lower.tail = FALSE)) # p-value

# Alternative White test ----

# Regression for alternative White test
model_Alt <- lm(uhat1sq ~ yhat1 + yhat1sq, hprice1)
summary(model_Alt)

# Number of independent variables k3
(k3 <- model_Alt$rank - 1)

# F-test and LM-test for heteroscedasticity
(r2 <- summary(model_Alt)$r.squared) # R-squared
(n <- nobs(model_Alt)) # number of observations

( F_stat <- (r2/k3) / ((1-r2)/(n-k3-1)) ) # F-statistic
( F_pval <- pf(F_stat, k3, n-k3-1, lower.tail = FALSE) ) # p-value

( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k3, lower.tail = FALSE)) # p-value

# All tests show homoscedasticity for lprice 


# Heteroscedasticity robust standard errors -------------------------------

# Robust standard errors correct for heteroscedasticity

# Regression model for price
# model_0 <- lm(price ~ lotsize + sqrft + bdrms, hprice1)
summary(model_0)

# Regression model for price with robust standard errors
coeftest(model_0, vcov. = vcovHC(model_0, type = "HC1"))
# Same coefficients, but heteroscedasticity consistent standard errors

# Regression model for log price
# model_2 <- lm(lprice ~ lotsize + sqrft + bdrms, hprice1)
summary(model_2)

# Regression model for log price with robust standard errors
coeftest(model_2, vcov. = vcovHC(model_2, type = "HC1"))
# Robust standard errors are not needed since log price is homoscedastic 


# Weighted Least Squares (WLS) --------------------------------------------
  
# When the heteroscedasticity form is known var(u|x)=(sigma^2)*(sqrft), 
# use WLS with weight=1/sqrft.

# WLS: estimate model with weight=1/sqrft 
model_WLS1 <- lm(formula = price ~ lotsize + sqrft + bdrms, 
                data = hprice1, weights = 1/sqrft)
summary(model_WLS1)

# Multiply all variables and the constant by 1/sqrt(sqrft)
hprice1 %<>% mutate(pricestar = price/sqrt(sqrft),
                    lotsizestar = lotsize/sqrt(sqrft),
                    sqrftstar = sqrft/sqrt(sqrft),
                    bdrmsstar = bdrms/sqrt(sqrft), 
                    constantstar = 1/sqrt(sqrft))

#  WLS: estimate model with transformed variables by OLS
model_WLS2 <- lm(pricestar ~ 0 + constantstar+ lotsizestar + sqrftstar + bdrmsstar, 
                hprice1)
summary(model_WLS2)


# Feasible GLS (FGLS) -----------------------------------------------------

# When the heteroscedasticity form is not known, 
# var(u|x) = sigma^2*(delta0 + delta1*lotsize + delta2*sqrft + delta3*bdrms)
# estimate hhat and use WLS with weight=1/hhat.

# Heteroscedasticity form, estimate hhat
# model_0 <- lm(price ~ lotsize + sqrft + bdrms, hprice1)
summary(model_0)
hprice1 %<>% mutate(u = resid(model_0), 
                    g = log(u^2))
model_g <- lm(g ~ lotsize + sqrft + bdrms, hprice1)
hprice1 %<>% mutate(ghat = fitted(model_g),
                    hhat = exp(ghat))

# FGLS: estimate model using WLS with weight=1/hhat
model_FGLS1 <- lm(formula = price ~ lotsize + sqrft + bdrms, 
                 data = hprice1, 
                 weights = 1/hhat)
summary(model_FGLS1)

# Multiply all variables and the constant by 1/sqrt(hhat)
hprice1 %<>% mutate(pricestar1 = price/sqrt(hhat),
                    lotsizestar1 = lotsize/sqrt(hhat),
                    sqrftstar1 = sqrft/sqrt(hhat),
                    bdrmsstar1 = bdrms/sqrt(hhat),
                    constantstar1 = 1/sqrt(hhat))

# FGLS: estimate model with transformed variables by OLS
model_FGLS2 <- lm(pricestar1 ~ 0 + constantstar1 + lotsizestar1 + sqrftstar1 + 
                    bdrmsstar1, hprice1)
summary(model_FGLS2)
