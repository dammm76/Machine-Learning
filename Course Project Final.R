#
#
#
#
#### Project ####

library(caret)

startData <- read.csv("C:\\Users\\Dave\\Documents\\Machine Learning\\pml-training.csv", header = TRUE)

set.seed(548799)

inTrain <- createDataPartition(y=startData$classe, p=0.7, list=FALSE)

training <- startData[inTrain,]
testing <- startData[-inTrain,]


### Cross validation


### This removes 59 variables with near zero variance
nsv<-nearZeroVar(training,saveMetrics = TRUE)

colsToKeep<-which(nsv$nzv==FALSE)

training2<-training[,colsToKeep]
###

### This removes preodmitely NA cols

naCounter<-NULL
for(i in 1:length(training2)){
  
  testCol<-which(is.na(training2[,i]))
  
  naCounter[i]<-length(testCol)  
  
  
}

colsToKeep<-which(naCounter<=nrow(training2)/2)

training3<-training2[,colsToKeep]

###

### This removes the dates and the counter type variables

training4<-training3[,-c(1,3:6)]

###

### model training (using rpart or gbm)
### gbm accuracy = 96%

modFit <- train(classe~.,data = training4, method = "gbm", trControl = trainControl(method = "cv"))

modFit$finalModel
summary(modFit$finalModel)

predTraining <-predict(modFit, newdata = training4)
confusionMatrix(data = predTraining,reference = training4$classe)

predTest <-predict(modFit, newdata = testing)
confusionMatrix(data = predTest,reference = testing$classe)

realTestData <- read.csv("C:\\Users\\Dave\\Documents\\Machine Learning\\pml-testing.csv", header = TRUE)

predReal <- predict(modFit, newdata = realTestData)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("C:\\Users\\Dave\\Documents\\Machine Learning\\problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predReal)

