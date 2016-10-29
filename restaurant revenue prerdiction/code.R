rm(list = ls())
setwd("path to working drirectory")

library(lubridate)
library(caret)
library(gbm)

train <- read.csv('train.csv', header = TRUE)
test <- read.csv('test.csv',header = TRUE)

summary(train)
#Outliers are not significant

plot(train$revenue)

####Fetch the year of opening
train$year <- year(as.POSIXct(train$Open.Date, format="%m/%d/%Y"))
test$year <- year(as.POSIXct(test$Open.Date, format="%m/%d/%Y"))

####Fetchin City.Group and transposing to columns
train$Big.Cities <- with(train, ifelse ((City.Group == "Big Cities"), 1, 0))
train$Other.Cities <- with(train, ifelse ((City.Group == "Other"), 1, 0))

test$Big.Cities <- with(test, ifelse ((City.Group == "Big Cities"), 1, 0))
test$Other.Cities <- with(test, ifelse ((City.Group == "Other"), 1, 0))

#Fetchin Type and transposing to columns
train$DT <- with(train, ifelse ((Type == "DT"), 1, 0))
train$FC <- with(train, ifelse ((Type == "FC"), 1, 0))
train$IL <- with(train, ifelse ((Type == "IL"), 1, 0))
train$MB <- with(train, ifelse ((Type == "MB"), 1, 0))

test$DT <- with(test, ifelse ((Type == "DT"), 1, 0))
test$FC <- with(test, ifelse ((Type == "FC"), 1, 0))
test$IL <- with(test, ifelse ((Type == "IL"), 1, 0))
test$MB <- with(test, ifelse ((Type == "MB"), 1, 0))


#separating dependant and independant variables in train and test data
trainX <- subset(train[,6:50], select = -c(revenue)) 
trainY <- subset(train, select = c(revenue))
testX <- test[,6:49]

##Find highly correlated variables and eliminate them
highCorr <- findCorrelation(cor(rbind(trainX, testX, use="pairwise", method="spearman")), cutoff = .80, verbose = TRUE) 

trainXFiltered <- trainX [,-highCorr]
testXFiltered <- testX [,-highCorr]

set.seed(1234)

###k fold cross validation for gbm

index <- createFolds(trainY[,1], returnTrain=TRUE)
cntrl <- trainControl(method ="cv", index=index)

gbm1 <- train(x = subset(trainXFiltered, select = - c(MB, DT)), y=trainY$revenue , method= "gbm", trControl=cntrl)
gbm1

########################BUILDING THE MODEL (gbm)

train_data_model <- 
  cbind(subset(trainXFiltered), revenue=trainY$revenue)

model <- gbm(revenue~., data= train_data_model, 
             n.trees=50, 
             interaction.depth=3,
             shrinkage= 0.1,
             distribution="gaussian")

TestFit <- data.frame(testXFiltered)

prediction <- predict.gbm(model, TestFit, 
                          type="response", 
                          n.trees=50, 
                          interaction.depth=3,
                          shrinkage= 0.1)

submit <- data.frame(Id = test$Id, Prediction=prediction)

write.csv(submit, file="submission.csv", row.names=FALSE)

###k fold cross validation for randomForest

rf1 <- train(x = subset(trainXFiltered), y=trainY$revenue , method= "rf", trControl=cntrl)
rf1

########################BUILDING THE MODEL (randomForest)

Fit <- randomForest(revenue~.
                              , data=trainNew, importance=TRUE, ntree=500, mtry=2)

#varImpPlot(Fit)

TestFit <- data.frame(testXFiltered)

predict <- predict(Fit, TestFit)

submit <- data.frame(Id = test$Id, Prediction=predict)

write.csv(submit, file="submission.csv", row.names=FALSE)
