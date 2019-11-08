library(tidyverse)
library(magrittr)
library(abind)
library(esquisse)

#Creates a function that reads in a file, turns missing cells into NA values
# and then goes through a cleaning process.
# The cleaning process draws the names for the first 5 columns from the first row
# and the names for the remaining from the second (where Questions are).
# Then it removes the first two rows and replaces the column names.
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
survey.nobio <- cbind(personinfo,surveyanswers)

#Read in Bio
bio <- read.csv("Bio DATA-Table 1.csv",na.strings=c(NA,""))
bio1 <- bio[,6:ncol(bio)]
vals<-which(!is.na(bio1[1,]))
qnames<-c("Q1.1","Q1.2","Q1.3","Q1.4","Q1.5","Q1.6","Q1.7","Q1.8","Q1.9","Q1.10",
          "Q2.1","Q2.2","Q2.3","Q2.4","Q2.5","Q2.6","Q2.7","Q2.8","Q2.9","Q2.10",
          "Q3.1","Q3.2","Q3.3","Q3.4","Q3.5","Q3.6","Q3.7","Q3.8","Q3.9",
          "Q4.1","Q4.2",
          "Q5","Q6","Q7","Q8","Q9","Q10",
          "Q11.1","Q11.2","Q11.3","Q11.4","Q11.5","Q11.6","Q11.7","Q11.8","Q11.9","Q11.10",
          "Q11.11","Q11.12","Q11.13","Q11.14","Q11.15","Q11.16",
          "Q12","Q13","Q14",
          "Q15.1","Q15.2","Q15.3","Q15.4","Q15.5","Q15.6","Q15.7",
          "Q16","Q17",
          "Q18")

bioans <- bio1[3:nrow(bio1),]
colnames(bioans) <- qnames

completesurvey <- cbind(personinfo,bioans,surveyanswers)


