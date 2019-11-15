library(tidyverse)
library(magrittr)
library(abind)
library(lavaan)

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
#PL.omit <- na.omit(data.PL)
PL.factor <-lapply(data.PL,function(x) as.numeric(as.factor(x)))
PL.factor <- as.data.frame(PL.factor)


library(lavaan)
PL.model <- 'pl1 =~ PL1a_Confidence + PL1b_Confidence + PL1c_Confidence + PL1d_Confidence + PL1e_Confidence + PL1f_Confidence + PL1g_Confidence
pl2 =~ PL2a_Confidence + PL2b_Confidence + PL2c_Confidence
pl3 =~ PL3a_Confidence + PL3b_Confidence + PL3c_Confidence + PL3d_Confidence + PL3e_Confidence
pl4 =~ PL4a_Confidence + PL4b_Confidence + PL4c_Confidence
pl5 =~ PL5a_Confidence + PL5b_Confidence + PL5c_Confidence + PL5d_Confidence
pl6 =~ PL6a_Confidence + PL6b_Confidence + PL6c_Confidence + PL6d_Confidence
pl7 =~ PL7a_Confidence + PL7b_Confidence + PL7c_Confidence
pl8 =~ PL8a_Confidence + PL8b_Confidence + PL8c_Confidence + PL8d_Confidence'


fit <- cfa(PL.model,data=PL.factor)
summary(fit)


