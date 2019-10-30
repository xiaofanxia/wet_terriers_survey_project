survey <- read.csv("survey.csv")
#survey$Start.Date <- gsub("^\\d ", "",survey$Start.Date)
survey$Start.Date <- as.numeric(as.POSIXct(survey$Start.Date)) %% 86400
