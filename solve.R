library(caret)
library(rpart)
library(e1071)

library(doMC)
registerDoMC(cores = 2)

set.seed(998)
testingSet <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
inTrain <- createDataPartition(y=testingSet$classe, p=0.7, list=F)
training <- testingSet[inTrain,]
test <- testingSet[-inTrain,]
dim(training); dim(test)
removeColumnsWithNA <- function(x) { x[,colSums(is.na(x))==0] }
training <- removeColumnsWithNA(training)
test <- removeColumnsWithNA(test)
ncol(training); ncol(test)
trainingData <- training[,8:59]
trainingOut <- training[,60]
modelRf <- train(trainingData,
                        trainingOut,
                        tuneGrid=data.frame(mtry=10),
                        trControl=trainControl(method="none")
                 )
testData <- test[,8:59]
testOut <- test[,60]
pred <- predict(modelRf, newdata=testData)

print(confusionMatrix(predict(modelRf,newdata=testData),testOut))
# 
# modFit <- train(classe ~ ., method="rpart",data=training[,8:60])
# modRf <- train(classe ~ ., method="rf",data=training[,8:60])
# modLda <- train(classe ~ ., method="lda",data=training)
# modNb <- train(classe ~ ., method="nb",data=training)
# NaiveBayes
# #pred<-predict(modFit,newdata=test)
# #testing$predRight<-pred==training$classe
# #table(pred,training$classe)
# TrainData <- training[,-160]
# TrainClasses <- training[,160]
# knnFit1 <- train(TrainData, TrainClasses,
#                  method = "knn",
#                  preProcess = c("center", "scale"),
#                  tuneLength = 10,
#                  trControl = trainControl(method = "cv"))
# 
# knnFit2 <- train(TrainData, TrainClasses,
#                  method = "knn",
#                  preProcess = c("center", "scale"),
#                  tuneLength = 10, 
#                  trControl = trainControl(method = "boot"))
# 
# 
# library(MASS)
# nnetFit <- train(TrainData, TrainClasses,
#                  method = "nnet",
#                  preProcess = "range", 
#                  tuneLength = 2,
#                  trace = FALSE,
#                  maxit = 100)
# 
# modFit
# knnFit1
# knnFit2
# nnetFit