library(lavaan)
library(tidyverse)
library(magrittr)
library(dplyr) 
library(tidyr)
library(knitr)

survey <- read_csv("survey.csv")
LT <- select(survey,starts_with("LT"))
LTC <- select(LT,contains("Confidence"))
LT_factor <-lapply(LTC,function(x) as.numeric(as.factor(x)))
LT_factor <- as.data.frame(LT_factor)

LT1_model <- 'lt1 =~ LT1a_Confidence + LT1b_Confidence + LT1c_Confidence'
fit1 <- cfa(LT1_model,data = LT_factor)
summary(fit1,fit.measures=TRUE)
parameterEstimates(fit1,standardized = TRUE)
parameterEstimates(fit1, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

LT2_model <- 'lt2 =~ LT2a_Confidence + LT2b_Confidence + LT2c_Confidence'
fit2 <- cfa(LT2_model,data = LT_factor)
summary(fit2,fit.measures=TRUE)
parameterEstimates(fit2,standardized = TRUE)
parameterEstimates(fit2, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

LT3_model <- 'lt3 =~ LT3a_Confidence + LT3b_Confidence + LT3c_Confidence + LT3d_Confidence'
fit3 <- cfa(LT3_model,data = LT_factor)
summary(fit3,fit.measures=TRUE)
parameterEstimates(fit3,standardized = TRUE)
parameterEstimates(fit3, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

LT4_model <- 'lt4 =~ LT4a_Confidence + LT4b_Confidence + LT4c_Confidence'
fit4 <- cfa(LT4_model,data = LT_factor)
summary(fit4,fit.measures=TRUE)
parameterEstimates(fit4,standardized = TRUE)
parameterEstimates(fit4, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

LT5_model <- 'lt5 =~ LT5a_Confidence + LT5b_Confidence + LT5c_Confidence'
fit5 <- cfa(LT5_model,data = LT_factor)
summary(fit5,fit.measures=TRUE)
parameterEstimates(fit5,standardized = TRUE)
parameterEstimates(fit5, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

