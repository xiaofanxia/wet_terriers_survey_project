library(lubridate)
library(hms)
survey <- read.csv("survey.csv")
survey$Start.Date <- gsub("[0-9]*/[0-9]*/\\d* ","", survey$Start.Date)
survey$Start.Date <- gsub("[A-Z]", "",survey$Start.Date)
survey$Start.Date <- gsub(":", ".",survey$Start.Date)
survey$Start.Date <- gsub("[0-9]*-[0-9]*-\\d* ", "",survey$Start.Date)


survey$End.Date <- gsub("[0-9]*/[0-9]*/\\d* ", "",survey$End.Date)
survey$End.Date <- gsub("[A-Z]", "",survey$End.Date)
survey$End.Date <- gsub(":", ".",survey$End.Date)
survey$End.Date <- gsub("[0-9]*-[0-9]*-\\d* ", "",survey$End.Date)
dif <- suppressWarnings(as.numeric(survey$End.Date) - as.numeric(survey$Start.Date))

difftime(survey$Start.Date, survey$End.Date)
