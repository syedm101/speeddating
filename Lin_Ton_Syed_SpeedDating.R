#Libraries Needed
library(caret)
library(corrplot)
library(DMwR)
library(ggplot2)
library(reshape2)
library(plyr)
library(sqldf)
library(mlbench)
library(randomForest)
library(gmodels)
library(party)
library(C50)
library(RWeka)

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
correlationMatrix <- cor(date3[,-which(names(date3)
                         %in% c('id', 'iid', 'idg', 'partner', 'pid', 'from'))],
                         use = "complete.obs")
correlated <- findCorrelation(correlationMatrix, cutoff=correlation_cutoff)
correlationMatrix2 <- as.data.frame(correlationMatrix[,correlated])
print(correlated)

correlationMatrix2 <- subset(correlationMatrix2, (correlationMatrix2 >= correlation_cutoff))
#Obvious strong correlation between museums and art

#Graphical Visualization of Correlation
#Created a subset of the ratng factors
correlation_subset <- date3[,c(18:21,27:56)]
subset_correlationMatrix <- cor(correlation_subset, use = 'complete.obs')

#Visualization of the correlations within the subset of rating factors
#Allows for the visualization of 'trade-offs' when marking preferences
corrplot(subset_correlationMatrix, method = 'circle', type= 'lower', insig = 'blank', 
         main = 'Correlation of Rating Attributes', mar=c(0,0,1,0))

#Visualization of the correlations between all attributes (very difficult to see)
corrplot(correlationMatrix, method = 'circle', type = 'lower', insig = 'blank', 
         main = 'Comparison of the Correlation of All Attributes')

#Plot colors
cbPalette <- c("#ffb3e6", "#56B4E9") #Set colors for pink and blue to easily represent males and females in graphs

#### Beginning Data Exploration ####
female <- subset(date3, date3$gender == 0)
male <- subset(date3, date3$gender == 1)

#Males vs. Females Preferences
#Selecting only the unique responses (since there are repeats for each person)
means1 <- sqldf("select distinct iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1
               from date3")
#Aggregating by gender for mean values
means1 <- aggregate(means1[-1],by=list(means1$gender),mean, na.rm = TRUE)
means1 <- means1[,c('gender', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1')]
means1_diff <- means1[1,] - means1[2,] #Difference between 0 and 1
means1 <- rename(means1, c('attr1_1' = 'Attractiveness', 'sinc1_1' = 'Sincerity', 'intel1_1' = 'Intelligence', 
                 'fun1_1' = 'Fun', 'amb1_1' = 'Ambition', 'shar1_1' = 'Shared Interests'))
#Transposing
means1 <-melt(means1,id.vars="gender")
#Graphing
means1_graph <- ggplot(means1,aes(x=variable,y=value,fill=factor(gender)))+ geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values = cbPalette, name="Gender", breaks=c(0, 1), labels=c("Female", "Male"))+ xlab("Attribute")+ylab("Mean Rating") +
  ggtitle('Female vs. Male Preferences')

#Perceptions of the Opposite Sex (Males for Females)
means2m <- sqldf("select distinct iid, gender, attr2_1, sinc2_1, intel2_1, fun2_1, amb2_1, shar2_1
               from date3")
means2m <- aggregate(means2m[-1],by=list(means2m$gender),mean, na.rm = TRUE)
means2m <- means2m[,c('gender', 'attr2_1', 'sinc2_1', 'intel2_1', 'fun2_1', 'amb2_1', 'shar2_1')]
means2m <- subset(means2m, gender == 1)
means2m <- rename(means2m, c('attr2_1' = 'Attractiveness', 'sinc2_1' = 'Sincerity', 'intel2_1' = 'Intelligence', 
                           'fun2_1' = 'Fun', 'amb2_1' = 'Ambition', 'shar2_1' = 'Shared Interests'))

means2f <- sqldf("select distinct iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1
               from date3")
means2f <- aggregate(means2f[-1],by=list(means2f$gender),mean, na.rm = TRUE)
means2f <- means2f[,c('gender', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1')]
means2f <- subset(means2f, gender == 0)
means2f <- rename(means2f, c('attr1_1' = 'Attractiveness', 'sinc1_1' = 'Sincerity', 'intel1_1' = 'Intelligence', 
                             'fun1_1' = 'Fun', 'amb1_1' = 'Ambition', 'shar1_1' = 'Shared Interests'))

means2c <- rbind(means2f, means2m)
means2c_diff <- means2c[1,] - means2c[2,]
means2c <-melt(means2c,id.vars="gender")

means2_graph <- ggplot(means2c,aes(x=variable,y=value,fill=factor(gender)))+ geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values = cbPalette, name="Key", breaks=c(0, 1), labels=c("Want Females Want", "What Males Think Females Want")) + 
  xlab("Attribute")+ylab("Mean Rating") + ggtitle('Male Perceptions of the other Sex vs. Reality')

#Perceptions of the Opposite Sex (Females for Males)
means3f <- sqldf("select distinct iid, gender, attr2_1, sinc2_1, intel2_1, fun2_1, amb2_1, shar2_1
               from date3")
means3f <- aggregate(means3f[-1],by=list(means3f$gender),mean, na.rm = TRUE)
means3f <- subset(means3f, gender == 0)
means3f <- means3f[,c('gender', 'attr2_1', 'sinc2_1', 'intel2_1', 'fun2_1', 'amb2_1', 'shar2_1')]
means3f <- rename(means3f, c('attr2_1' = 'Attractiveness', 'sinc2_1' = 'Sincerity', 'intel2_1' = 'Intelligence', 
                             'fun2_1' = 'Fun', 'amb2_1' = 'Ambition', 'shar2_1' = 'Shared Interests'))

means3m <- sqldf("select distinct iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1
               from date3")
means3m <- aggregate(means3m[-1],by=list(means3m$gender),mean, na.rm = TRUE)
means3m <- means3m[,c('gender', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1')]
means3m <- subset(means3m, gender == 1)
means3m <- rename(means3m, c('attr1_1' = 'Attractiveness', 'sinc1_1' = 'Sincerity', 'intel1_1' = 'Intelligence', 
                             'fun1_1' = 'Fun', 'amb1_1' = 'Ambition', 'shar1_1' = 'Shared Interests'))

means3c <- rbind(means3f, means3m)
means3c_diff <- means3c[1,] - means3c[2,]
means3c <-melt(means3c,id.vars="gender")

cbPalette <- c("#56B4E9", "#ffb3e6")
means3_graph <- ggplot(means3c,aes(x=variable,y=value,fill=factor(gender)))+ geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values = cbPalette, name="Key", breaks=c(0, 1), labels=c("What Males Want", "What Females Think Males Want")) + 
  xlab("Attribute")+ylab("Mean Rating") + ggtitle('Female Perceptions of the other Sex vs. Reality')

#Who put 100 for attractiveness and how successful were they? Exploration for humor's sake
hundred_attr <- subset(date3, date3$attr1_1 == 100)

#All males marked 100 for desired attractiveness
hundred_attr_graph <- ggplot(hundred_attr,aes(factor(gender, levels = c(0,1)), ,fill=factor(gender)))+ geom_bar() + 
  scale_fill_manual(values = cbPalette, name="Key", breaks=c(0, 1), labels=c("Females", "Males")) + 
  xlab("Sex")+ylab("Count of Individuals") + ggtitle('Individuals who marked 100 for Desired Attractiveness') + 
  scale_x_discrete(drop=FALSE)

#How successful were they?
hundred_attr_graph_success <- ggplot(hundred_attr,aes(factor(match, levels = c(0,1)), ,fill=factor(match)))+ 
  geom_bar() + scale_fill_manual(values = cbPalette, name="Key", breaks=c(0, 1), labels=c("No Match", "Match")) + 
  xlab("Match")+ylab("Count of Matches") + ggtitle('Individuals who marked 100 for Desired Attractiveness, Outcomes') + 
  scale_x_discrete(drop=FALSE)

#Was it because they were really picky?
hundred_attr_graph_success2 <- ggplot(hundred_attr,aes(factor(dec, levels = c(0,1)), ,fill=factor(match)))+ geom_bar() + 
  scale_fill_manual(values = cbPalette, name="Key", breaks=c(0, 1), labels=c("Did not want to see again", "Wanted to see again")) + 
  xlab("Decision by Male")+ylab("Count of Decisions") + ggtitle('Individuals who marked 100 for Desired Attractiveness, Decisions') + 
  scale_x_discrete(drop=FALSE)


#Exporing Date Order in the Night and Matches--Best time/order to speed date?
match_time <- date3[, c("wave", "order", "match")]
match_time$wave <- factor(match_time$wave)
match_time2 <- aggregate(match~order+wave,data=match_time,FUN=sum)

##Scatter plot with number of matches vs. order, grouped by wave
time_scatter <- ggplot(data = match_time2, aes(x = order, y = match, colour = wave)) +       
                geom_jitter(aes(group = wave)) + geom_point() + ggtitle('Number of Matches vs. Order in the Night by Wave')

match_time3 <- match_time2
match_time3$order1 <- match_time3$order
match_time3$order <- NULL
match_time3 <- sqldf("select order1, wave, max(match) from match_time3 group by wave")

##We looked at each wave and the order in the wave that had the most matches. We then plotted the frequency of the occurances
##by order.
match_time_graph <- ggplot(match_time3,aes(order1)) + geom_bar() + xlab("Order")+ 
  ylab("Count where matches were a maximum") + ggtitle('Occurance of the Max Number of Matches in Each Wave')


match_time4 <- sqldf("select wave, avg(match) as MatchAvg from match_time2 group by wave")

##We looked at the Average Matches per order for all waves
match_time_graph2 <- ggplot(match_time4,aes(x=wave, y=MatchAvg)) + geom_bar(stat="identity") + xlab("Order")+ 
  ylab("Average Matches") + ggtitle('Average Matches per Order for all Waves')

#*************************************
#####Begin Machine learning process####

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

#One way to look at attribute importance
model <- train(dec_o ~., data=d_train, method="lvq", preProcess="scale")#, trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance, ylab = 'Attributes', main = 'Attribute Importance')

#Perform Feature Selection
# load the libraries
set.seed(7)
View(d_train[,-1])

control <- rfeControl(functions=rfFuncs, method="cv", number=10) #number = 10?
results <- rfe(d_train[,-1], d_train$dec_o, sizes=c(1:6), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"), main = "Recursive Feature Elimination Results")

#create simple c5 decision tree
#Using all attributes
c5_all <- C5.0(d_train[,-1],d_train$dec_o)
c5_all_pred <- predict(c5_all, d_test[,-1])
CrossTable(d_test$dec_o, c5_all_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))
#74.8% accuracy

#Using the top 2 (attractiveness and shared interests)
c5_2 <- C5.0(d_train[,c(2,7)],d_train$dec_o)
c5_2_pred <- predict(c5_2, d_test[,-1])
CrossTable(d_test$dec_o, c5_2_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))
#75.3% accuracy

#Don't factor the result and ambition (according to RFE)
c5_model <- C5.0(d_train[,c(-1,-6)],d_train$dec_o)

print(c5_model)
plot(c5_model)

#Predict testing results according to the model
c5_pred <- predict(c5_model, d_test[,-1])
#create crosstable matrix to assess performance

CrossTable(d_test$dec_o, c5_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))
#74.2% accuracy

tree_model = ctree(dec_o ~ ., d_train[,-6]) #Make sure model is running on right stuff
plot(tree_model)
ctree_pred <- predict(tree_model,d_test[,-1])
CrossTable(d_test$dec_o, ctree_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))
#75.3% accuracy

#Using JRIP
jrip_model <- JRip(dec_o~.,data=d_train)
jrip_model

jrip_pred <- predict(jrip_model, d_test)
CrossTable(d_test$dec_o, jrip_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))
#75.5% accuracy