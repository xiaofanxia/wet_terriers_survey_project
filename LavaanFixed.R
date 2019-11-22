library(tidyverse)
library(magrittr)
library(abind)
library(lavaan)
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

#Reads in the file names from the current repository and selects
#the ones with a character pattern (picked TELL Statements) that identifies
#the files you want to read in. Then, make a blank list of vectors to make
# an array for each one of these files.
filenames <- list.files()
selectfiles <- filenames[grep("TELL Statements",filenames,perl=FALSE)]
resultlist <- vector("list",length(selectfiles))

#Reads in and cleans each file and puts them into one cell of the array.
for(i in 1:length(selectfiles)) {
  resultlist[[i]] <- rowclean(selectfiles[i])
}

#Stack them all together. 
aggregate <- abind(resultlist, along=2,force.array=F)

#At this point there are multiple sets of columns with personal info, which
#makes it messier and more repetitive. These next few code pieces identify
# the column names, remove all of them, and add in one set at the start.
personinfo <- aggregate[,1:5]
surveyanswers <- aggregate[,-which(names(aggregate) %in% names(personinfo))]
survey <- cbind(personinfo,surveyanswers)



colnames(surveyanswers)[c(grep("PL1a _Confidence",colnames(surveyanswers)))] <- "PL1a_Confidence"
colnames(surveyanswers)[c(grep("PL6c_Contributes.1",colnames(surveyanswers)))] <- "PL6c_Confidence"
data.PL <- surveyanswers[,c(grep("PL",colnames(surveyanswers)))]
data.PL <- data.PL[,c(grep("Confidence",colnames(data.PL)))]
ans = c("I'm not sure what this means", "Not confident at all","Somewhat Confident","Neutral","Confident","Very Confident")
PL.ordered <- data.PL
PL.numer <- data.PL
for (i in 1:ncol(data.PL)) {
  PL.ordered[,i] <- factor(data.PL[,i],levels=ans)
  PL.numer[,i] <- as.numeric(PL.ordered[,i])
}

##The Lavaan Stuff
PL.model <- 'pl1 =~ PL1a_Confidence + PL1b_Confidence + PL1c_Confidence + PL1d_Confidence + PL1e_Confidence + PL1f_Confidence + PL1g_Confidence
pl2 =~ PL2a_Confidence + PL2b_Confidence + PL2c_Confidence
pl3 =~ PL3a_Confidence + PL3b_Confidence + PL3c_Confidence + PL3d_Confidence + PL3e_Confidence
pl4 =~ PL4a_Confidence + PL4b_Confidence + PL4c_Confidence
pl5 =~ PL5a_Confidence + PL5b_Confidence + PL5c_Confidence + PL5d_Confidence
pl6 =~ PL6a_Confidence + PL6b_Confidence + PL6c_Confidence + PL6d_Confidence
pl7 =~ PL7a_Confidence + PL7b_Confidence + PL7c_Confidence
pl8 =~ PL8a_Confidence + PL8b_Confidence + PL8c_Confidence + PL8d_Confidence'

PL1 <- 'pl1 =~ PL1a_Confidence + PL1b_Confidence + PL1c_Confidence + PL1d_Confidence + PL1e_Confidence + PL1f_Confidence + PL1g_Confidence'
fit1 <- cfa(PL1,data=PL.numer)
parameterEstimates(fit1,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

PL1.1 <- 'pl1 =~ PL1a_Confidence + PL1c_Confidence + PL1e_Confidence + PL1f_Confidence'
fit1.1 <- cfa(PL1.1,data=PL.numer)
parameterEstimates(fit1.1,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

PL2 <- 'pl2 =~ PL2a_Confidence + PL2b_Confidence + PL2c_Confidence'
fit2 <- cfa(PL2,PL.numer)
parameterEstimates(fit2,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

PL2.1 <- 'pl2 =~ PL2a_Confidence + PL2b_Confidence'
fit2.1 <- cfa(PL2.1,PL.numer)
parameterEstimates(fit2.1,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")


## Something is wrong with including PL3e_Confidence.
PL3 <- 'pl3 =~ PL3a_Confidence +  PL3c_Confidence'
fit3 <- cfa(PL3,PL.numer)
parameterEstimates(fit3,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")


PL4 <- 'pl4 =~ PL4a_Confidence + PL4b_Confidence + PL4c_Confidence'
fit4 <- cfa(PL4,PL.numer)
parameterEstimates(fit4,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

PL5 <- 'pl5 =~ PL5a_Confidence + PL5b_Confidence + PL5c_Confidence'
fit5 <- cfa(PL5,PL.numer)
parameterEstimates(fit5,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

PL6 <- 'pl6 =~ PL6a_Confidence + PL6b_Confidence + PL6c_Confidence'
fit6 <- cfa(PL6,PL.numer)
parameterEstimates(fit6,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

PL7 <- 'pl7 =~ PL7a_Confidence + PL7b_Confidence + PL7c_Confidence'
fit7 <- cfa(PL7,PL.numer)
parameterEstimates(fit7,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

PL8 <- 'pl8 =~ PL8a_Confidence + PL8b_Confidence + PL8c_Confidence'
fit8 <- cfa(PL8,PL.numer)
parameterEstimates(fit8,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

fit <- cfa(PL.model,data=PL.numer)
parameterEstimates(fit,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

PL.new <- '
pl1 =~ PL1a_Confidence + PL1c_Confidence + PL1e_Confidence + PL1f_Confidence
pl2 =~ PL2a_Confidence + PL2b_Confidence + PL2c_Confidence
pl3 =~ PL3a_Confidence + PL3b_Confidence
pl4 =~ PL4a_Confidence + PL4b_Confidence + PL4c_Confidence
pl5 =~ PL5a_Confidence + PL5b_Confidence + PL5c_Confidence
pl6 =~ PL6a_Confidence + PL6b_Confidence + PL6c_Confidence
pl7 =~ PL7a_Confidence + PL7b_Confidence + PL7c_Confidence
pl8 =~ PL8a_Confidence + PL8b_Confidence + PL8c_Confidence
'
fit.new <- cfa(PL.new, data=PL.numer)
parameterEstimates(fit.new,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
