#Import Data
#Data should be sourced within the project, using relative path
date_original <- read.csv("Speed Dating Data.csv", stringsAsFactors = FALSE)
View(date_original)
summary(date_original)

#Blank Strings
date_original[date_original == ""] <- NA

#Attributes with more than around 1/3 of the data missing
c(names(which(colSums(is.na(date_original))>3000)))
date <- date_original[,-which(names(date_original) %in% names(which(colSums(is.na(date_original))>3000)))]

#Removed commas in variables
date[] <- lapply(date, function(x) gsub(",","", x)) #Not sure if this is completely needed

#Removed attributes that did not appear to be useful
date <- date[,-which(names(date) %in%
                       c('condtn', 'position', 'positin1', 'undergrd', 'field', 'career'))]

#Viewing ones on a different scale -- appears data from 1 to 10 has been re-scaled
different_scale <- subset(date, wave %in% c(6,7,8,9))