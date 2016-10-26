#Libraries Needed
library(caret)
library(corrplot)
library(DMwR)

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

#Removed attributes that did not appear to be useful
date <- date[,-which(names(date) %in%
                       c('condtn', 'position', 'positin1', 'undergrd', 'field', 'career', 'zipcode', 'goal',
                         'date', 'go_out'))]


#Viewing ones on a different scale -- appears data from 1 to 10 has been re-scaled
different_scale <- subset(date, date$wave %in% c(6,7,8,9))

#Rearranged columns to place relevant/matching information side-by-side
#Removed post-date follow-up Time2 metrics other than satis_2
date2 <- date[,c(1:9, 12:14, 31, 33, 34, 35, 11, 10, 21, 79, 88, 30, 89:92, 15:20, 68:73, 56:61, 62:67, 22:27, 80:85,
                 28, 86, 29, 87, 74:78, 93:98, 32, 37, 36, 38:55)]

#Viewing differences in column names to ensure that everything carried over properly
setdiff(names(date), names(date2))

#Viewing observations with many NAs
manyNAs(date2,0.2)
date3 <- date2[-manyNAs(date2),]

#Viewing correlations
correlation_cutoff <- 0.75
correlationMatrix <- cor(date[,-which(names(date)
                         %in% c('id', 'iid', 'idg', 'partner', 'pid', 'from'))],
                         use = "complete.obs")
correlated <- findCorrelation(correlationMatrix, cutoff=correlation_cutoff)
correlationMatrix2 <- as.data.frame(correlationMatrix)
print(correlated)

correlationMatrix2 <- correlationMatrix2[,correlated]
correlationMatrix2 <- subset(correlationMatrix2, (attr1_2 >= correlation_cutoff| attr3_2  >= correlation_cutoff
                                                  | amb3_2  >= correlation_cutoff | museums  >= correlation_cutoff))

#Graphical Visualization of Correlation
#Created a subset of the ratng factors
correlation_subset <- date[,c(15:27,60:89)]
subset_correlationMatrix <- cor(correlation_subset, use = 'complete.obs')

#Visualization of the correlations within the subset of rating factors
#Allows for the visualization of 'trade-offs' when marking preferences
corrplot(subset_correlationMatrix, method = 'circle', type= 'lower', insig = 'blank', main = 'Correlation of Rating Attributes')

#Created another subset for direct comparison (acutal vs. perceived interests, participant vs. partner ratings)
correlation_subset2 <- date[,c(15:20, 72:77, 21:27, 83:89)]
subset_correlationMatrix2 <- cor(correlation_subset2, use = 'complete.obs')

#Comparison of correlation with rating attributes
corrplot(subset_correlationMatrix2, method = 'circle', type= 'lower', insig = 'blank', 
         main = 'Correlation of Rating Attributes')

#Visualization of the correlations between all attributes (very difficult to see)
corrplot(correlationMatrix, method = 'circle', type = 'lower', insig = 'blank', 
         main = 'Comparison of the Correlation of All Attributes')
