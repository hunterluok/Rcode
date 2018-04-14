library(rpart)
library(rpart.plot)
library(caret)
bh<-read.data
set.seed(1000)
t.idx<-createDataPartition(bh$MEDV,p=0.7,list=FALSE)
bfit<-rpart(MEDV~.,data=bh[t.idx,])
bfit
prp(bfit,type=2,nn=TRUE,fallen.leaves=TRUE,faclen=4,varlen=8,shadow.col="red")
bfit$cptable
plotcp(bfit)

bfitprune<-prune(bfit,cp=0.01192653)

prp(bfitprune,type=2,nn=TRUE,fallen.leaves=TRUE,faclen=4,varlen=8,shadow.col="blue")
preds.t<-predict(bfitprune,bh[t.idx,])
sqrt(mean((preds.t-bh[t.idx,"MEDV"])^2))
pred<-predict(bfitprune,bh[-t.idx,])
sqrt(mean((pred-bh[-t.idx,"MEDV"])^2))
fit<-rpart(MEDV~.,data=bh[t.idx,],control=rpart.control(minsplit=10,cp=0.001,minbucket=5))