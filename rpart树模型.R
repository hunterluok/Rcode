#删除 不必要的变量

churntrain<-churnTrain[,!names(churnTrain) %in% c("state","area_code","account_length")]
head(churntrain)

#构建数据集

set.seed(2)
# library(caret)
# ind1<-createDataPartition(churntrain$churn,p=0.7,list=FALSE)
# ind2<-sample(nrow(churntrain),0.7*nrow(churntrain))
ind<-sample(2,nrow(churntrain),replace=TRUE,prob=c(0.7,0.3))

traindata<-churntrain[ind==1,]
testdata<-churntrain[ind==2,]


splitdata<-function(data,p,s){
set.seed(s)
index<-sample(1:dim(data)[1])
train<-data[index[1:floor(dim(data)[1]*p)],]
test<-data[index[((ceiling(dim(data)[1]*p))+1):dim(data)[1]],]
return(list(train=train,test=test))
}

library(rpart)
model<-rpart(churn~.,traindata)
names(model)

#画图
plot(model,margin=0.1)
text(model,all=TRUE,use.n=TRUE)
plot(model,uniform=TRUE,branch=0.6,margin=0.1)
text(model,all=TRUE,use.n=TRUE)

pred<-predict(model,testdata,type="class")
table(testdata$churn,pred,dnn=c("real","pred"))
confusionMatrix(table(pred,testdata$churn))
