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
ans = c("I'm not sure what this means", "Not confident at all","Somewhat confident","Neutral","Confident","Very Confident")
PL.ordered <- data.PL
PL.numer <- data.PL
for (i in 1:ncol(data.PL)) {
  PL.ordered[,i] <- factor(data.PL[,i],levels=ans)
  PL.numer[,i] <- as.numeric(PL.ordered[,i])
}
colnames(PL.numer) <- colnames(data.PL)


#Lavaan Stuff

PL1 <- 'pl1 =~ PL1a_Confidence + PL1b_Confidence + PL1c_Confidence + PL1d_Confidence + PL1f_Confidence
PL1a_Confidence ~~ PL1f_Confidence'
fit1 <- cfa(PL1,data=PL.numer,std.lv=T) 
summary(fit1,standardized=T)
fitMeasures(fit1) #Look at cfi and tli. Ideally they should both be close to/above 0.9.
modificationIndices(fit1) # If you want to look at correlation, look at the "~~" terms.
parameterEstimates(fit1,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
PL1table <- c("PL1","PL1a,PL1b,PL1c,PL1d,PL1f",0.887, round(fitMeasures(fit1)[c("cfi")],digits=3),round(fitMeasures(fit1)[c("tli")],digits=3))


PL2 <- 'pl2 =~ aa*PL2a_Confidence + aa*PL2b_Confidence + PL2c_Confidence'
fit2 <- cfa(PL2,data=PL.numer,std.lv=T) #EStimate Error Variances
summary(fit2,standardized=T) # 6 parameters for each model. In each thing there would be 3 manifest items (6 unique pieces)
fitMeasures(fit2) #No degrees of freedom if there are three questions.
modificationIndices(fit2) #Lambda for each thing. Pick the two closest Lambdas and then set them to be the same with aa*
parameterEstimates(fit2,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
PL2table <- c("PL2","PL2a,PL2b,PL2c",0.292, round(fitMeasures(fit2)[c("cfi")],digits=3),round(fitMeasures(fit2)[c("tli")],digits=3))

PL3 <- 'pl3 =~ PL3a_Confidence + aa*PL3d_Confidence + aa*PL3e_Confidence'
fit3 <- cfa(PL3,data=PL.numer,std.lv=T) #EStimate Error Variances
summary(fit3,standardized=T) # 6 parameters for each model. In each thing there would be 3 manifest items (6 unique pieces)
fitMeasures(fit3) #No degrees of freedom if there are three questions.
modificationIndices(fit3) #Lambda for each thing. Pick the two closest Lambdas and then set them to be the same with aa*
parameterEstimates(fit3,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
PL3table <- c("PL3","PL3a,PL3d,PL3e",0.902, round(fitMeasures(fit3)[c("cfi")],digits=3),round(fitMeasures(fit3)[c("tli")],digits=3))


PL4 <- 'pl4 =~ aa*PL4a_Confidence + aa*PL4b_Confidence + PL4c_Confidence'
fit4 <- cfa(PL4,data=PL.numer,std.lv=T) #EStimate Error Variances
summary(fit4,standardized=T) # 6 parameters for each model. In each thing there would be 3 manifest items (6 unique pieces)#No degrees of freedom if there are three questions.
fitMeasures(fit4)
modificationIndices(fit4) #Lambda for each thing. Pick the two closest Lambdas and then set them to be the same with aa*
parameterEstimates(fit4,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
PL4table <- c("PL4","PL4a,PL4b,PL4c",0.051, round(fitMeasures(fit4)[c("cfi")],digits=3),round(fitMeasures(fit4)[c("tli")],digits=3))

PL5 <- 'pl5 =~ PL5a_Confidence + PL5b_Confidence + PL5c_Confidence + PL5d_Confidence'
fit5 <- cfa(PL5,data=PL.numer,std.lv=T) #EStimate Error Variances
summary(fit5,standardized=T) # 6 parameters for each model. In each thing there would be 3 manifest items (6 unique pieces)
fitMeasures(fit5) #No degrees of freedom if there are three questions.
modificationIndices(fit5) #Lambda for each thing. Pick the two closest Lambdas and then set them to be the same with aa*
parameterEstimates(fit5,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
PL5table <- c("PL5","PL5a,PL5b,PL5c,PL5d",0.261, round(fitMeasures(fit5)[c("cfi")],digits=3),round(fitMeasures(fit5)[c("tli")],digits=3))


PL6 <- 'pl6 =~ aa*PL6a_Confidence + aa*PL6b_Confidence + PL6c_Confidence'
fit6 <- cfa(PL6,data=PL.numer,std.lv=T) #EStimate Error Variances
summary(fit6,standardized=T) # 6 parameters for each model. In each thing there would be 3 manifest items (6 unique pieces)
fitMeasures(fit6) #No degrees of freedom if there are three questions.
modificationIndices(fit6) #Lambda for each thing. Pick the two closest Lambdas and then set them to be the same with aa*
parameterEstimates(fit6,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
PL6table <- c("PL6","PL6a,PL6b,PL6c",0.283, round(fitMeasures(fit6)[c("cfi")],digits=3),round(fitMeasures(fit6)[c("tli")],digits=3))

PL7 <- 'pl7 =~ aa*PL7a_Confidence + PL7b_Confidence + aa*PL7c_Confidence'
fit7 <- cfa(PL7,data=PL.numer,std.lv=T) #EStimate Error Variances
summary(fit7,standardized=T) # 6 parameters for each model. In each thing there would be 3 manifest items (6 unique pieces)
fitMeasures(fit7) #No degrees of freedom if there are three questions.
modificationIndices(fit7) #Lambda for each thing. Pick the t∆wo closest Lambdas and then set them to be the same with aa*
parameterEstimates(fit7,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
PL7table <- c("PL7","PL7a,PL7b,PL7c",0.903, round(fitMeasures(fit7)[c("cfi")],digits=3),round(fitMeasures(fit7)[c("tli")],digits=3))


PL8 <- 'pl8 =~ PL8a_Confidence + aa*PL8b_Confidence + aa*PL8c_Confidence'
fit8 <- cfa(PL8,data=PL.numer,std.lv=T) #EStimate Error Variances
summary(fit8,standardized=T) # 6 parameters for each model. In each thing there would be 3 manifest items (6 unique pieces)
fitMeasures(fit8) #No degrees of freedom if there are three questions.
modificationIndices(fit8) #Lambda for each thing. Pick the two closest Lambdas and then set them to be the same with aa*
parameterEstimates(fit8,standardized=T) %>%
  filter(op=="=~") %>%
  select('Latent Factor'=lhs,Indicator=rhs,B=est,SE=se,Z=z,'p-value'=pvalue,loading=std.all) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
PL8table <- c("PL8","PL8a,PL8b,PL8c",0.301, round(fitMeasures(fit8)[c("cfi")],digits=3),round(fitMeasures(fit8)[c("tli")],digits=3))
round(fitMeasures(fit8)[c("cfi")],digits=3)

PLTable <- rbind(PL1table,PL2table,PL3table,PL4table,PL5table,PL6table,PL7table,PL8table)
colnames(PLTable) <- c("Section","Questions","P-Value","CFI","TLI")
rownames(PLTable) <- NULL
kable(PLTable,digits=3,format="pandoc",caption="'Planning' Subdomain Summary")



