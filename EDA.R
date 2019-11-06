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
graph

min(survey$num)
#0 min
max(survey$num)
#200 min = 3.3 hours
mean(survey$num) 
#111.5 min = 1.85 hours
median(survey$num)
#134 min = 2.2 hours

#Transform one single value to a vector corresponding to a created variable:
survey %>% separate()
  
  
  