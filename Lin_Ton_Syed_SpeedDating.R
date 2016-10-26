#Libraries Needed
library(caret)
library(corrplot)
library(DMwR)
library(ggplot2)
library(reshape2)
library(plyr)

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
#Removed the 4_1 variables
date2 <- date[,c(1:9, 12:14, 31, 33, 34, 35, 11, 10, 21, 79, 88, 30, 89:92, 15:20, 68:73, 56:61, 22:27, 80:85,
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

#Plot colors
cbPalette <- c("#ffb3e6", "#56B4E9")


#### Beginning Data Exploration ####
female <- subset(date3, date3$gender == 0)
male <- subset(date3, date3$gender == 1)

#Males vs. Females Preferences
means1 <- aggregate(date3,by=list(date3$gender),mean, na.rm = TRUE)
means1 <- means1[,c('gender', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1')]
means1 <- rename(means1, c('attr1_1' = 'Attractiveness', 'sinc1_1' = 'Sincerity', 'intel1_1' = 'Intelligence', 
                 'fun1_1' = 'Fun', 'amb1_1' = 'Ambition', 'shar1_1' = 'Shared Interests'))
means1 <-melt(means1,id.vars="gender")

means1_graph <- ggplot(means1,aes(x=variable,y=value,fill=factor(gender)))+ geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values = cbPalette, name="Gender", breaks=c(0, 1), labels=c("Female", "Male"))+ xlab("Attribute")+ylab("Mean Rating") +
  ggtitle('Female vs. Male Preferences')

#Perceptions of the Opposite Sex (Males for Females)
means2m <- aggregate(date3,by=list(date3$gender),mean, na.rm = TRUE)
means2m <- means2m[,c('gender', 'attr2_1', 'sinc2_1', 'intel2_1', 'fun2_1', 'amb2_1', 'shar2_1')]
means2m <- subset(means2m, gender == 1)
means2m <- rename(means2m, c('attr2_1' = 'Attractiveness', 'sinc2_1' = 'Sincerity', 'intel2_1' = 'Intelligence', 
                           'fun2_1' = 'Fun', 'amb2_1' = 'Ambition', 'shar2_1' = 'Shared Interests'))

means2f <- aggregate(date3,by=list(date3$gender),mean, na.rm = TRUE)
means2f <- means2f[,c('gender', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1')]
means2f <- subset(means2f, gender == 0)
means2f <- rename(means2f, c('attr1_1' = 'Attractiveness', 'sinc1_1' = 'Sincerity', 'intel1_1' = 'Intelligence', 
                             'fun1_1' = 'Fun', 'amb1_1' = 'Ambition', 'shar1_1' = 'Shared Interests'))

means2c <- rbind(means2m, means2f)
means2c <-melt(means2c,id.vars="gender")

means2_graph <- ggplot(means2c,aes(x=variable,y=value,fill=factor(gender)))+ geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values = cbPalette, name="Key", breaks=c(0, 1), labels=c("Want Females Want", "What Males Think Females Want")) + 
  xlab("Attribute")+ylab("Mean Rating") + ggtitle('Male Perceptions of the other Sex vs. Reality')

#Perceptions of the Opposite Sex (Females for Males)
means3f <- aggregate(date3,by=list(date3$gender),mean, na.rm = TRUE)
means3f <- means3f[,c('gender', 'attr2_1', 'sinc2_1', 'intel2_1', 'fun2_1', 'amb2_1', 'shar2_1')]
means3f <- subset(means3f, gender == 0)
means3f <- rename(means3f, c('attr2_1' = 'Attractiveness', 'sinc2_1' = 'Sincerity', 'intel2_1' = 'Intelligence', 
                             'fun2_1' = 'Fun', 'amb2_1' = 'Ambition', 'shar2_1' = 'Shared Interests'))

means3m <- aggregate(date3,by=list(date3$gender),mean, na.rm = TRUE)
means3m <- means3m[,c('gender', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1')]
means3m <- subset(means3m, gender == 1)
means3m <- rename(means3m, c('attr1_1' = 'Attractiveness', 'sinc1_1' = 'Sincerity', 'intel1_1' = 'Intelligence', 
                             'fun1_1' = 'Fun', 'amb1_1' = 'Ambition', 'shar1_1' = 'Shared Interests'))

means3c <- rbind(means3m, means3f)
means3c <-melt(means3c,id.vars="gender")

cbPalette <- c("#56B4E9", "#ffb3e6")
means3_graph <- ggplot(means3c,aes(x=variable,y=value,fill=factor(gender)))+ geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values = cbPalette, name="Key", breaks=c(0, 1), labels=c("What Males Want", "What Females Think Males Want")) + 
  xlab("Attribute")+ylab("Mean Rating") + ggtitle('Female Perceptions of the other Sex vs. Reality')
