library(tidyverse)
library(lavaan)
library(magrittr)
library(kableExtra)

#ignore contribute part, only focus on confidence in Performance & Feedback domain:
PF <- read.csv("survey.csv")
PF <- select(PF, starts_with("PF"))
conf <- select(PF, contains("confidence"))

#Convert character choice to numeric:
new <- apply(conf,2,function(x) as.numeric(as.factor(x)))
PF_factor <- as.data.frame(new)

# first subdomain
m1 <- 'PF1 =~ PF1a_Confidence + PF1b_Confidence + PF1c_Confidence + PF1d_Confidence + PF1e_Confidence'
fit1 <- cfa(m1,data = PF_factor, std.lv=TRUE)
#summary(fit1,fit.measures=TRUE,standardized=TRUE)
parameterEstimates(fit1, standardized=TRUE)

parameterEstimates(fit1, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

# second subdomain
m2 <- 'PF2 =~ PF2a_Confidence + PF2b_Confidence + PF2c_Confidence + PF2d_Confidence + PF2e_Confidence'
fit2 <- cfa(m2,data = PF_factor, std.lv=TRUE)
#summary(fit1,fit.measures=TRUE,standardized=TRUE)
parameterEstimates(fit2, standardized=TRUE)

parameterEstimates(fit2, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

# third subdomain
m3 <- 'PF3 =~ PF3a_Confidence + PF3b_Confidence + PF3c_Confidence + PF3d_Confidence + PF3e_Confidence'
fit3 <- cfa(m3,data = PF_factor, std.lv=TRUE)
#summary(fit1,fit.measures=TRUE,standardized=TRUE)
parameterEstimates(fit3, standardized=TRUE)

parameterEstimates(fit3, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

# fourth subdomain
m4 <- 'PF4 =~ PF4a_Confidence + PF4b_Confidence'
fit4 <- cfa(m4,data = PF_factor, std.lv=TRUE)
#summary(fit1,fit.measures=TRUE,standardized=TRUE)
parameterEstimates(fit4, standardized=TRUE)

parameterEstimates(fit4, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

# fifth subdomain
m5 <- 'PF5 =~ PF5a_Confidence + PF5b_Confidence + PF5c_Confidence'
fit5 <- cfa(m5,data = PF_factor, std.lv=TRUE)
#summary(fit1,fit.measures=TRUE,standardized=TRUE)
parameterEstimates(fit5, standardized=TRUE)

parameterEstimates(fit5, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

########
measure <- fitMeasures(fit, fit.measures = "all", baseline.model = NULL) 
#standardize cfa fit:
s <- standardizedSolution(fit)

summary(fit4)

#Check p-value, if p-value > 0.05, check loadings, if loadings > 0.6, keep it, then 


