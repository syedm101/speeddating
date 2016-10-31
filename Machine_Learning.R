#Libraries Needed
library(caret)
library(corrplot)
library(DMwR)
library(ggplot2)
library(reshape2)
library(plyr)

#Import Data
#Data should be sourced within the project, using relative path
date_original <- read.csv("Speed Dating Data.csv", stringsAsFactors = TRUE)
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

#*************************************
#Begin Machine learning process

#Classify the decision of a partner based on their rating of you
#The classifier is "dec_o" and the attributes "attr_o	sinc_o	intel_o	fun_o	amb_o	shar_o"

#returns the column number (so I can easily subset the data)
grep("^dec_o$", colnames(date3) ) #19th column
grep("^attr_o$", colnames(date3) ) #45th column
grep("^shar_o$", colnames(date3) ) #50th column
View(date3)

#subset data and view it to make sure it seems right
rel_data = date3[,c(19,45:50)]
View(rel_data)#Has around 8000 rows

sum(is.na(rel_data)) #Need to be wary of NAs
rel_data <- na.omit(rel_data) #remove all rows with NAs (still have ~7000 rows after this operation)

#Randomly order the data
set.seed(7)
rel_data <- rel_data[order(runif(6965)), ]
#Set dec_o as factor
rel_data$dec_o <- factor(rel_data$dec_o)

#Split the data ~80 training 20 testing
d_train <- rel_data[(1:5500),]
d_test <- rel_data[5500:6965,]


#Perform Feature Selection
# load the libraries
library(mlbench)
library(caret)
library(randomForest)
View(d_train[,-1])
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10) #number = 10?
# run the RFE algorithm
results <- rfe(d_train[,-1], d_train$dec_o, sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#create simple c5 decision tree
library(C50)
#Don't factor the result and ambition (according to RFE)
c5_model <- C5.0(d_train[,c(-1,-6)],d_train$dec_o)

print(c5_model)
plot(c5_model)

#Predict testing results according to the model
c5_pred <- predict(c5_model, d_test[,-1])
#create crosstable matrix to assess performance
library(gmodels)
CrossTable(d_test$dec_o, c5_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))
#has ~0.742 accuracy

library(party)
tree_model = ctree(dec_o ~ ., d_train[,-6]) #Make sure model is running on right stuff
plot(tree_model)
ctree_pred <- predict(tree_model,d_test[,-1])
CrossTable(d_test$dec_o, ctree_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))
#Much less accuracy? interesting


