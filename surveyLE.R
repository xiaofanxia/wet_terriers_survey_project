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




PL.factor <- select(survey,'LE1a_Contributes':'LE6d_Confidence')
PL.factor <-lapply(PL.factor,function(x) as.numeric(as.factor(x)))
PL.factor <- as.data.frame(PL.factor)


library(lavaan)
LE1model<-'LE1 =~LE1a_Confidence+LE1b_Confidence+LE1c_Confidence+LE1d_Confidence+LE1e_Confidence'
fitLE1 <- cfa(LE1model,data=PL.factor)
summary(fitLE1)

LE2model<-'LE2 =~LE2a_Confidence+LE2b_Confidence+LE2c_Confidence+LE2d_Confidence+LE2e_Confidence+LE2f_Confidence'
fitLE2 <- cfa(LE2model,data=PL.factor)
summary(fitLE2)

LE3model<-'LE3 =~LE3a_Confidence+LE3b_Confidence+LE3c_Confidence+LE3d_Confidence+LE3e_Confidence+LE3f_Confidence+LE3g_Confidence'
fitLE3 <- cfa(LE3model,data=PL.factor)
summary(fitLE3)

LE4model<-'LE4 =~LE4a_Confidence+LE4b_Confidence+LE4c_Confidence+LE4d_Confidence+LE4e_Confidence'
fitLE4 <- cfa(LE4model,data=PL.factor)
summary(fitLE4)

LE5model<-'LE5 =~LE5a_Confidence+LE5b_Confidence+LE5c_Confidence+LE5d_Confidence'
fitLE5 <- cfa(LE5model,data=PL.factor)
summary(fitLE5)

LE6model<-'LE6 =~LE6a_Confidence+LE6b_Confidence+LE6c_Confidence+LE6d_Confidence'
fitLE6 <- cfa(LE6model,data=PL.factor)
summary(fitLE6)


