# Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

#Include below Libraries

library(ggplot2)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(lattice)
library(e1071)
library(caret)
library(rattle)
library(pander)
library(randomForest)

# Set working directory

setwd("C:\\Users\\globalworld\\Desktop\\Machine_Learning\\Project")

# Read data sources

training <- read.csv("pml-training.csv",na.strings=c("NA",""))
testing <-read.csv("pml-testing.csv",na.strings=c("NA",""))

dim(training)
dim(testing)
training[1:5,c('user_name','classe','num_window','roll_belt','pitch_belt')]

#Check the count of total 'NA' values in training and test datasets

sum(is.na(training))
t1 <- table(colSums(is.na(training)))
t2 <- table(colSums(is.na(testing)))
pandoc.table(t1, style = "grid", justify = 'left', caption = 'Training data column NA frequencies')

pandoc.table(t2, style = "grid", justify = 'left', caption = 'Testing data column NA frequencies')
columnNACounts <- colSums(is.na(training))        # getting NA counts for all columns
badColumns <- columnNACounts >= 19000             # ignoring columns with majority NA values
cleanTrainingdata <- training[!badColumns]        # getting clean data
sum(is.na(cleanTrainingdata))                     # checking for NA values


cleanTrainingdata <- cleanTrainingdata[, c(7:60)] # removing unnecessary columns

# for testing dataset
columnNACounts <- colSums(is.na(testing))         # getting NA counts for all columns
badColumns <- columnNACounts >= 20                # ignoring columns with majority NA values
cleanTestingdata <- testing[!badColumns]        # getting clean data
sum(is.na(cleanTestingdata))                     # checking for NA values
cleanTestingdata <- cleanTestingdata[, c(7:60)] # removing unnecessary columns

s <- summary(cleanTrainingdata$classe)

#With A,B,C,D,E

pandoc.table(s, style = "grid", justify = 'left', caption = '`classe` frequencies')
plot(cleanTrainingdata$classe,main = "`classe` frequency plot")

#Model Building

#Partition the cleanTrainingdata dataset into training and testing data sets for building our model 

partition <- createDataPartition(y = cleanTrainingdata$classe, p = 0.6, list = FALSE)
trainingdata <- cleanTrainingdata[partition, ]
testdata <- cleanTrainingdata[-partition, ]

#Using training dataset, build the model using the Random Forest machine learning algorithm.

model <- train(classe ~ ., data = trainingdata, method = "rf", prox = TRUE, trControl = trainControl(method = "cv", number = 4, allowParallel = TRUE))
model
# In Sample Accuracy
# In sample accuracy which is the prediction accuracy of our model on the training data set.

training_pred <- predict(model, trainingdata)
confusionMatrix(training_pred, trainingdata$classe)

#Out of Sample Accuracy using Test dataset
testing_pred <- predict(model, testdata)
confusionMatrix(testing_pred, testdata$classe)


# Prediction Assignment for 20 test cases in the testing dataset
answers <- predict(model, cleanTestingdata)
answers <- as.character(answers)
answers

pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

pml_write_files(answers)


# Conclusion

#Algorithm Used : Random Forest
#Can handle thousands of variables
#Highly accurate
#Random Forest model is difficult to interpret
#Processing is slow
