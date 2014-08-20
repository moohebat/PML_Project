library(caret)

training<-ns

#removing zero variance
nsv<-nearZeroVar(training,saveMetrics=TRUE)
tmp<-training[,!nsv[,4]]
#removing the participant name and time (6 first columns)
tmp<-tmp[,-c(1:6)]
#removing sparcity of NA
removeNullColumn<-function(data){
  loop<-dim(data)[2]
  index<-logical(loop)
  for(i in 1:loop){
    if(sum(is.na(data[,i]))>(length(data[,i])/2)){
      index[i]<-TRUE
    }
  }
  data[,!index]
}

tmp<-removeNullColumn(tmp)
#create data partition
sepIndex<-createDataPartition(tmp$classe,p=0.60,list=FALSE)
#create test and training set
trainingset<-tmp[sepIndex,]
testingset<-tmp[-sepIndex,]
#cross validation
ctrl<-trainControl(method="cv",number=10)
#create the model fit
modelFit<-train(classe~.,method="rf",data=trainingset,trControl=ctrl)

print(modelFit$finalModel)

prediction<-predict(modelFit,newdata=testingset)

confusionMatrix(prediction, testingset$classe)
