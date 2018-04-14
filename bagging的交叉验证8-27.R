library(C50)
data(churn)
head(churnTrain)
set.seed(2)
ind<-sample(2,nrow(churnTrain),replace=TRUE,prob=c(0.7,0.3))
trainset=churnTrain[ind==1,]
testset=churnTrain[ind==2,]
dim(trainset);dim(testset);

library(adabag)
set.seed(2)
model1<-bagging(churn~.,trainset,mfinal=10)
names(model1)

sort(model1$importance,decreasing=T)
pred<-predict(model1,testset)
pred$confusion
pred$error
pred$names

install.packages("ipred")
library(ipred)
model2<-bagging(churn~.,trainset,coob=T)
mean(predict(model2)!=trainset$churn)
pred1<-predict(model2,testset,type="class") #这里注意是分类结果
table(pred1,testset$churn)

#bagging的交叉验证
model3<-bagging.cv(churn~.,v=10,mfinal=10,trainset)
names(model3)
model3$confusion
model3$error










