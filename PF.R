library(tidyverse)
library(lavaan)
library(magrittr)
library(kableExtra)

rowclean <- function(filename) {
  data <- read.csv(paste0(filename),na.string=c("","NA"))
  rownum <- max(which(is.na(data[2,])==F))
  clean <- data[,1:rownum]
  colnames(clean) = NULL
  names1 <- clean[1,1:5]
  names2 <- clean[2,6:rownum]
  names<- cbind(names1, names2)
  clean <- clean[3:nrow(clean),]
  colnames(clean) <- c(t(names))
  return(clean)
}
filenames <- list.files()
selectfiles <- filenames[grep("TELL Statements",filenames,perl=FALSE)]
resultlist <- vector("list",length(selectfiles))

for(i in 1:length(selectfiles)) {
  resultlist[[i]] <- rowclean(selectfiles[i])
}
aggregate <- abind(resultlist, along=2,force.array=F)
personinfo <- aggregate[,1:5]
surveyanswers <- aggregate[,-which(names(aggregate) %in% names(personinfo))]
survey <- cbind(personinfo,surveyanswers)
colnames(surveyanswers)[c(grep("PF1a _Confidence",colnames(surveyanswers)))] <- "PF1a_Confidence"
colnames(surveyanswers)[c(grep("PF5c_Contributes.1",colnames(surveyanswers)))] <- "PF5c_Confidence"

data.PF <- surveyanswers[,c(grep("PF",colnames(surveyanswers)))]
data.PF <- data.PF[,c(grep("Confidence",colnames(data.PF)))]
ans = c("I'm not sure what this means", "Not confident at all","Somewhat confident","Neutral","Confident","Very Confident")
PF.ordered <- data.PF
PF.number <- data.PF
for (i in 1:ncol(data.PF)) {
  PF.ordered[,i] <- factor(data.PF[,i],levels=ans)
  PF.number[,i] <- as.numeric(PF.ordered[,i])
}
colnames(PF.number) <- colnames(data.PF)


# First subdomain
m1 <- 'PF1 =~ PF1a_Confidence + PF1b_Confidence + PF1c_Confidence + PF1d_Confidence + PF1e_Confidence'
fit1 <- cfa(m1,data = PF.number, std.lv=TRUE)
#summary(fit1,fit.measures=TRUE,standardized=TRUE)
summary(fit1,standardized=T)
#P-value = 0.008
parameterEstimates(fit1, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#Factor loadings: 0.609; 0.830; 0.946; 0.915; 0.587

#Since p-value of first subdomain is 0.008 < 0.05, and the factor loadings of "PF1e_Confidence" is lowest, thus, we try to drop it from the first subdomain
m1.1 <- 'PF1 =~ PF1a_Confidence + PF1b_Confidence + PF1c_Confidence + PF1d_Confidence'
fit1.1 <- cfa(m1.1,data = PF.number, std.lv=TRUE)
summary(fit1.1,standardized=T)
#P-value = 0.967 > 0.05, thus we do not need to change any more on the first subdomain.


# Second subdomain
m2 <- 'PF2 =~ PF2a_Confidence + PF2b_Confidence + PF2c_Confidence + PF2d_Confidence + PF2e_Confidence'
fit2 <- cfa(m2,data = PF.number, std.lv=TRUE)
summary(fit2,standardized=T)
#P-value = 0.013
parameterEstimates(fit2, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#Factor loadings: 0.587; 0.905; 0.558; 0.901; 0.896

#Since p-value of first subdomain is 0.013 < 0.05, and the factor loadings of "PF2c_Confidence" is lowest, thus, we try to drop it from the second subdomain
m2.1 <- m2 <- 'PF2 =~ PF2a_Confidence + PF2b_Confidence + PF2d_Confidence + PF2e_Confidence'
fit2.1 <- cfa(m2.1,data = PF.number, std.lv=TRUE)
summary(fit2.1,standardized=T)
#P-value = 0.459 > 0.05, thus we can stay here for the second subdomain.


# Third subdomain
m3 <- 'PF3 =~ PF3a_Confidence + PF3b_Confidence + PF3c_Confidence + PF3d_Confidence + PF3e_Confidence'
fit3 <- cfa(m3,data = PF.number, std.lv=TRUE)
#summary(fit1,fit.measures=TRUE,standardized=TRUE)
summary(fit3,standardized=T)
#P-value = 0.712 
parameterEstimates(fit3, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#Factor loadings: 0.485; 0.838; 0.746; 0.767; 0.588
#Since p-value > 0.05, third subdomain is ok.


# Fourth subdomain
m4 <- 'PF4 =~ PF4a_Confidence + PF4b_Confidence
       PF5 =~ PF5a_Confidence + PF5b_Confidence + PF5c_Confidence'
fit4 <- cfa(m4,data = PF.number, std.lv=TRUE)
#summary(fit1,fit.measures=TRUE,standardized=TRUE)
summary(fit4,standardized=T)
#P-value = 0.012 
parameterEstimates(fit4, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#Factor loadings: 0.888; 0.835; 0.757; 0.759; 0.567

#Since P-value is 0.012 < 0.05, and the lowest factor loading is "PF5c_Confidence", thus we try to drop it from the subdomain
m4.1 <- 'PF4 =~ PF4a_Confidence + PF4b_Confidence
       PF5 =~ PF5a_Confidence + PF5b_Confidence'
fit4.1 <- cfa(m4.1,data = PF.number, std.lv=TRUE)
summary(fit4.1,standardized=T)
#P-value is 0.362 > 0.05.

# 
# # Fifth subdomain
# m5 <- 'PF5 =~ PF5a_Confidence + PF5b_Confidence + PF5c_Confidence'
# fit5 <- cfa(m1,data = PF.number, std.lv=TRUE)
# #summary(fit1,fit.measures=TRUE,standardized=TRUE)
# summary(fit5,standardized=T)
# #P-value = 0.008
# 
# parameterEstimates(fit5, standardized=TRUE) %>% 
#   filter(op == "=~") %>% 
#   select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, loading=std.all) %>% 
#   kable(digits = 3, format="pandoc", caption="Factor Loadings")
# #Factor loadings: 0.609; 0.83; 0.946; 0.915; 0.587

#Check p-value, if p-value > 0.05, check loadings, if loadings > 0.6, keep it

