library(tidyverse)
library(lavaan)
library(magrittr)

#ignore contribute part, only focus on confidence in Performance & Feedback domain:
PF <- read.csv("survey.csv")
PF <- select(PF, starts_with("PF"))
conf <- select(na, contains("confidence"))

#Convert character choice to numeric:
new <- apply(conf,2,function(x) as.numeric(as.factor(x)))

#Fit model as latent variable(left hand side) VS indicators(right hand side):
PF1 =~ PF1a_Confidence + PF1b_Confidence + PF1c_Confidence + PF1d_Confidence + PF1e_Confidence
PF2 =~ PF2a_Confidence + PF2b_Confidence + PF2c_Confidence + PF2d_Confidence + PF2e_Confidence
PF3 =~ PF3a_Confidence + PF3b_Confidence + PF3c_Confidence + PF3d_Confidence + PF3e_Confidence
PF4 =~ PF4a_Confidence + PF4b_Confidence 
PF5 =~ PF5a_Confidence + PF5b_Confidence + PF5c_Confidence
model <- 'PF1 =~ PF1a_Confidence + PF1b_Confidence + PF1c_Confidence + PF1d_Confidence + PF1e_Confidence
PF2 =~ PF2a_Confidence + PF2b_Confidence + PF2c_Confidence + PF2d_Confidence + PF2e_Confidence
PF3 =~ PF3a_Confidence + PF3b_Confidence + PF3c_Confidence + PF3d_Confidence + PF3e_Confidence
PF4 =~ PF4a_Confidence + PF4b_Confidence 
PF5 =~ PF5a_Confidence + PF5b_Confidence + PF5c_Confidence'
fit <- cfa(model, data=new)
summary(fit, fit.measures=TRUE) #fit.measures=TRUE mean contains additional fit measures(start with Model test baseline model)
#From summary result, p-value of PF3d_Confidence and PF5a_Confidence is larger than 0.05

measure <- fitMeasures(fit, fit.measures = "all", baseline.model = NULL) 
#standardize cfa fit:
s <- standardizedSolution(fit)






