library(lubridate)
library(hms)
library(ggplot2)
survey <- read.csv("survey.csv")


#Uniform newest part time into POSIX objects:
survey$Start.Date <- strptime(survey$Start.Date, format = "%m/%d/%Y %H:%M:%S")
survey$End.Date <- strptime(survey$End.Date, format = "%m/%d/%Y %H:%M:%S")
#Time people used to finish the survey:
survey$time <- abs(difftime(survey$End.Date, survey$Start.Date, units = "mins"))

#Count number of NA values in rows:
survey$num <- apply(survey, MARGIN = 1, FUN=function(x) length(x[is.na(x)]))

data <- select(survey, Start.Date, End.Date, time)
#graph the plot:
graph <- ggplot(survey, aes(x=time, y=num)) +
  geom_point()

graph <- graph + labs(x = "Time people used in survey(min)", y = "Number of questions they didn't answer") 



##################useless draft(might be)

#Uniform the format of time:
survey$Start.Date <- gsub("[0-9]*/[0-9]*/\\d* ","", survey$Start.Date)
survey$Start.Date <- gsub("[A-Z]", "",survey$Start.Date)
#survey$Start.Date <- gsub(":", ".",survey$Start.Date)
survey$Start.Date <- gsub("[0-9]*-[0-9]*-\\d* ", "",survey$Start.Date)


survey$End.Date <- gsub("[0-9]*/[0-9]*/\\d* ", "",survey$End.Date)
survey$End.Date <- gsub("[A-Z]", "",survey$End.Date)
#survey$End.Date <- gsub(":", ".",survey$End.Date)
survey$End.Date <- gsub("[0-9]*-[0-9]*-\\d* ", "",survey$End.Date)
dif <- suppressWarnings(as.numeric(survey$End.Date) - as.numeric(survey$Start.Date))
