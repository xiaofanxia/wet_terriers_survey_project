library(lavaan)
library(tidyverse)
library(magrittr)
library(dplyr) 
library(tidyr)
library(knitr)
library(abind)
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
colnames(surveyanswers)[c(grep("LT1a _Confidence",colnames(surveyanswers)))] <- "LT1a_Confidence"
colnames(surveyanswers)[c(grep("LT5c_Contributes.1",colnames(surveyanswers)))] <- "LT56c_Confidence"

data.LT <- surveyanswers[,c(grep("LT",colnames(surveyanswers)))]
data.LT <- data.LT[,c(grep("Confidence",colnames(data.LT)))]
ans = c("I'm not sure what this means", "Not confident at all","Somewhat confident","Neutral","Confident","Very Confident")
LT.ordered <- data.LT
LT.numer <- data.LT
for (i in 1:ncol(data.LT)) {
  LT.ordered[,i] <- factor(data.LT[,i],levels=ans)
  LT.numer[,i] <- as.numeric(LT.ordered[,i])
}
colnames(LT.numer) <- colnames(data.LT)
#survey <- read_csv("survey.csv")
#LT <- select(survey,starts_with("LT"))
#LTC <- select(LT,contains("Confidence"))
#data.LT <-lapply(LTC,function(x) as.numeric(as.factor(x)))
#data.LT <- as.data.frame(data.LT)

LT1 <- 'lt1 =~ aa*LT1a_Confidence + aa*LT1b_Confidence + LT1c_Confidence '
fit1 <- cfa(LT1,data=LT.numer,std.lv=T)
summary(fit1,standardized=T)
fitMeasures(fit1)
modificationIndices(fit1)
parameterEstimates(fit1,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.741>0.05. Factor loadings: a-0.488;b-0.472;c-0.722. cfi=1;ltli=1.607.

LT2 <- 'lt2 =~ aa*LT2a_Confidence + aa*LT2b_Confidence + LT2c_Confidence'
fit2 <- cfa(LT2,data=LT.numer,std.lv=T)
summary(fit2,standardized=T)
fitMeasures(fit2)
modificationIndices(fit2)
parameterEstimates(fit2,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.953>0.05. Factor loadings: a-0.587;b-0.603;c-0.817. cfi=1; tli=1.234.

LT3 <- 'lt3 =~ LT3a_Confidence + aa*LT3b_Confidence + aa*LT3d_Confidence'
fit3 <- cfa(LT3,data=LT.numer,std.lv=T)
summary(fit3,standardized=T)
fitMeasures(fit3)
modificationIndices(fit3)
parameterEstimates(fit3,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#Initial p-value with 4 questions is 0.008. 
#After dropping question c, the p-value=0.897. Factor loadings:a-0.959;b-0.802;c-0.629. cfi=1;tli=1.087.

LT4 <- 'lt4 =~ LT4a_Confidence + aa*LT4b_Confidence + aa*LT4c_Confidence'
fit4 <- cfa(LT4,data=LT.numer,std.lv=T)
summary(fit4,standardized=T)
fitMeasures(fit4)
modificationIndices(fit4)
parameterEstimates(fit4,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.899. Factor loadings: a-0.469;b-1.012;c-0.886. cfi=1; tli=1.06.


LT5 <- 'lt5 =~ aa*LT5a_Confidence + LT5b_Confidence + aa*LT5c_Confidence'
fit5 <- cfa(LT5,data=LT.numer,std.lv=T)
summary(fit5,standardized=T)
fitMeasures(fit5)
modificationIndices(fit5)
parameterEstimates(fit5,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
#P-value=0.379. Factor loadings: a-0.618;b-0.844;c-0.532. cfi=1; tli=1.055.
