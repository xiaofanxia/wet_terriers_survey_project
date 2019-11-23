library(lavaan)
library(tidyverse)
library(magrittr)
library(dplyr) 
library(tidyr)
library(knitr)

survey <- read_csv("survey.csv")
LT <- select(survey,starts_with("LT"))
LTC <- select(LT,contains("Confidence"))
data.LT <-lapply(LTC,function(x) as.numeric(as.factor(x)))
data.LT <- as.data.frame(data.LT)

LT1_model <- 'lt1 =~ LT1a_Confidence + LT1b_Confidence + LT1c_Confidence '
fit1 <- cfa(LT1_model,data =data.LT)
summary(fit1,fit.measures=TRUE)
parameterEstimates(fit1,standardized = TRUE)
parameterEstimates(fit1, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

#P-value=0.182>0.05. All three questions in subdomain LT1 coule be saved. 

LT2_model <- 'lt2 =~ LT2a_Confidence + LT2b_Confidence + LT2c_Confidence'
fit2 <- cfa(LT2_model,data =data.LT)
summary(fit2,fit.measures=TRUE)
parameterEstimates(fit2,standardized = TRUE)
parameterEstimates(fit2, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.04<0.05. 

#lt2b and lt2c have closer lambda.
LT2new_model <- 'lt2 =~ LT2a_Confidence + cc*LT2b_Confidence + cc*LT2c_Confidence'
fit2new <- cfa(LT2new_model,data = data.LT)
summary(fit2new,fit.measures=TRUE)
parameterEstimates(fit2new,standardized = TRUE)
parameterEstimates(fit2new, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.04; Lambda: lt2-a=1.041； Question a could be saved. Question b&c have lambda less than 0.05. 


LT3_model <- 'lt3 =~ LT3a_Confidence + LT3b_Confidence + LT3c_Confidence + LT3d_Confidence'
fit3 <- cfa(LT3_model,data =data.LT)
summary(fit3,fit.measures=TRUE)
parameterEstimates(fit3,standardized = TRUE)
parameterEstimates(fit3, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.112>0.05. All three questions in subdomain LT3 coule be saved. 

LT4_model <- 'lt4 =~ LT4a_Confidence + LT4b_Confidence + LT4c_Confidence'
fit4 <- cfa(LT4_model,data =data.LT)
summary(fit4,fit.measures=TRUE)
parameterEstimates(fit4,standardized = TRUE)
parameterEstimates(fit4, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.01<0.05.

LT4_new_model <- 'lt4 =~ LT4a_Confidence + cc*LT4b_Confidence + cc*LT4c_Confidence'
fit4new <- cfa(LT4_new_model,data =data.LT)
summary(fit4new,fit.measures=TRUE)
parameterEstimates(fit4new,standardized = TRUE)
parameterEstimates(fit4new, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.01；Lambda: lt4-b=0.785,lt4-c=0.726; Question b and c could be saved.

LT5_model <- 'lt5 =~ LT5a_Confidence + LT5b_Confidence + LT5c_Confidence'
fit5 <- cfa(LT5_model,data = data.LT)
summary(fit5,fit.measures=TRUE)
parameterEstimates(fit5,standardized = TRUE)
parameterEstimates(fit5, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.344>0.05. All questions in subdomain LT-5 could be saved. 
