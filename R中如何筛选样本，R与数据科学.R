library(caret)

training<-createDataPartition(data$mpg,p=0.75,list=FALSE)
trainingData<-data[training,]
testData<-data[-training,]
model3<-knn(train=trainingData,test=testData,cl=trainingData$mpg)

completedata<-data[complete.cases(data),]

##方法1
drops<-c("car.name")
completeData2<-completedata[,!(names(completedata) %in% drops)]
training<-createDataPartition(completeData2$mpg,p=0.75,list=FALSE)
trainingData<-completeData2[training,]
testData<-completeData2[-training,]
model3<-knn(train=trainingData,test=testData,cl=trainingData$mpg)

##方法2
index<-1:nrow(spam)
testindex<-sample(index,trunc(length(index)/3))
testset<-spam[testindex,]
trainingset<-spam[-testindex,]
