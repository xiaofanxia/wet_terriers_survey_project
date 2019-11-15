library(tidyverse)
PF <- read.csv("survey.csv")
PF <- select(PF, starts_with("PF"))
conf <- select(na, contains("confidence"))


