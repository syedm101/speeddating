#Import Data
#Data should be sourced within the project, using relative path
date_original <- read.csv("Speed Dating Data.csv", stringsAsFactors = FALSE)
View(date_original)
summary(date_original)

#Attributes with more than around 1/3 of the data missing:
#expnum, attr5_1, sinc5_1, intel5_1, fun5_1, amb5_1, attr1_s, sinc1_s, intel1_s, fun1_s, amb1_s, shar1_s, attr3_s,
#sinc3_s, intel3_s, fun3_s, amb3_s
#Test
which(colMeans(is.na(date_original)) > 1000)
