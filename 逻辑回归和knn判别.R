#knn算法 churn数据集

library(C50)
data(churn)
head(churnTrain)

vars<-names(churntrain)[1:2];vars

levels(churntrain[,1])=list("0"="no","1"="yes")  #这个地方很有用

set.seed(2)
ind<-sample(2,nrow(churntrain),replace=TRUE,prob=c(0.7,0.3))
train<-churntrain[ind==1,]
test<-churntrain[ind==2,]   #这里注意

model1<-knn(train[,!names(train) %in% c("churn")],test[,!names(test)%in% c("churn")],train$churn,k=3)

kknn带权重的 KNN

#逻辑回归

model2<-glm(churn~.,train,family=binomial)

summary(model2)

#找出对模型有显著影响的变量

names(model2)
#寻找变量的P值小于0.05
t<-summary(model2)$coefficients
t1<-which(t[,4]<0.05)
vars<-names(train[t1-1])  #减去1为 去掉借据项

formula1<-as.formula(paste("churn",paste(vars,collapse='+'),sep='~')) #这个方法注意
formula1
model3<-glm(formula1,train,family=binomial)

pred<-predict(model3,test,type="response")
Class<-pred>.05
summary(Class)
tb<-table(test$churn,Class)
pred.mod<-ifelse(test$churn=="yes",1,0)
pred_class<-churn.mod
pred_class[pred<=0.5]=1-pred_class[pred<=0.5]
ctb<-table(churn.mod,pred_class)
ctb






