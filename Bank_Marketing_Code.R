#Importing libraries
library(caret)
library(e1071)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)

#Importing Data Set
Data <- read.csv2('C:/bank-full.csv',header=TRUE)

#Exploring Data
summary(Data)
#Check for NA is any
which(is.na(Data), arr.ind = TRUE)
#check for types of columns
str(Data)

#Checking Correlation in numeric variables
cor(Data[,c(1,6,10,12,13,14,15)])

#Splitting Data into Train & Test
set.seed(1)
sample = sample.split(Data$age, SplitRatio = .75)
train_data = subset(Data, sample == TRUE)
test_data  = subset(Data, sample == FALSE)

#Making Decision Tree as first model #90.01% Accuracy
model_dtree <- rpart(y~., data=train_data) 
fancyRpartPlot(model_dtree)
predictions_dtree <- predict(model_dtree, test_data[,-17], type = "class")
confusionMatrix(test_data$y,predictions_dtree)

#Making RandomForest 
model_rf<-train(y~.,data=train_data,method='ranger')
predictions_rf <- predict(model_rf, test_data[,-17])
confusionMatrix(test_data$y,predictions_rf)

#Training a KNN $90.12% Accuracy
model_lr<-train(y~.,data=train_data,method='glm')
predictions_lr <- predict(model_lr, test_data[,-17])
confusionMatrix(test_data$y,predictions_lr)

#Training a svm with linear kernel
model_svm<-train(y~.,data=train_data,method='svmRadial')
predictions_svm <- predict(model_svm, test_data[,-17])
confusionMatrix(test_data$y,predictions_svm)
