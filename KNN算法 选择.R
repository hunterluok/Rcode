

f<-function(traindata,testdata,   )

re1<-knn(traindata,testdata,traindata$result,k)

re2<-knn(traindata,tttdata,traindata$result,k)

table(re1,testdata$result,dnn=c("pred","real"))

table(re2,tttdata$result,dnn=c("pred","real"))


##KNN自动筛选，自变量为数值型，因变量为 分类型。变量进行标准化。
library(class)
library(caret)
#样本分为3个区
pred<-knn(train,test,trainresult,k,prob=T)

knnauto<-function(traindata,testdata,trainresult,testresult,start,end)
{
for(i in start:end){
pred<-knn(traindata,testdata,trainresult,i)
tab<-table(pred,testresult,dnn=c("pre","real"))
cat(paste("Erroe matrix for i=",i,"\n"))
cat("============================\n")
print(tab)
cat("----------------------------\n\n\n")
}
}
knnauto(td[,4:5],vall[4:5],td[,3],vall[,3],1,7)


