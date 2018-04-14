#knn 回归预测

library(dummies)
library(FNN)
library(scales)

educ<-read.csv("Chapter4/education.csv")

library(caret)

dums<-dummy(educ$region,sep="_")
educ<-cbind(educ,dums)

#对数据进行标准化#

set.seed(1000)
ind<-createDataPartition(educ$expense,p=0.6,list=FLASE）
ind<-createDataPartition(educ$expense,p=0.6,list=FLASE)

library(caret)

ind<-createDataPartition(educ$expense,p=0.6,list=FALSE)
train<-educ[ind,]
test<-educ[-ind,]
set.seed(2000)
ind1<-createDataPartition(test$expense,list=FALSE,p=0.5)
val<-test[ind1,]
test1<-test[-ind1,]
dim(train);dim(val);dim(test1)

mod<-knn.reg(train[,c(7:12)],test1[,c(7:12)],train[,6],5,algorithm="brute")
rems1<-sqrt(mean(mod$pred-test1$expense)^2);rems1


mod1<-knn.reg(train[,c(7:12)],val[,c(7:12)],train[,6],4,algorithm="brute")
rems1<-sqrt(mean(mod1$pred-val$expense)^2);rems1







