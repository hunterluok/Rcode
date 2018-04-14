library(ISLR)
data(Caravan)
d<-Caravan
set.seed(1)
train<-sample(1:nrow(d),1000)
traindata<-d[train,]
testdata<-d[-train,]
library(gbm)
m1<-gbm(Purchase~.,data=traindata,distribution="bernoulli",n.trees=1000,shrinkage=0.01,verbose=F)
plot(m1)

最优子集选择
library(leaps)
ms<-regsubsets(Salary~.,Hitters)
sm<-regsubsets(Purchase~.,d)  #为什么没发运行呢？
summary(sm)

sm<-regsubsets(Purchase~.,d,method="forward",nvmax=dim(d)[2]-1) 
names(sm)
plot(sm$rss,type="l")
which.min(sm$rss)
points(86,sm$rss[86],col="red",cex=3,pch=20)
t<-sm$rss
set.seed(2)
rate<-rep(0,length(t)-1)
for(i in 1:length(rate)){
rate[i]<-ifelse((t[i]-t[i+1])/t[i]>0.001,1,0)
}
rate   #得到较之前改善程度超过0.1%的变量数为11
vars<-coef(sm,11) #11变量的模型。

vars<-vars[-1]  #去掉截距项。
selectvar<-names(vars)
newd<-d[,selectvar]    #选择最优预测变量进行建模。

newp<-ifelse(d$Purchase=="No",0,1) #进行 转换
newd<-data.frame(newd,newp)

train<-sample(1:nrow(newd),1000)
traindata<-newd[train,]
testdata<-newd[-train,]
library(gbm)
m1<-gbm(newp~.,data=traindata,distribution="bernoulli",n.trees=1000,shrinkage=0.01) #伯努利要求因变量为0-1.
import<-summary(m1)
dim(import)

pred<-ifelse(predict(m1,testdata,n.trees=1000,type="response")>0.2,"1","0")
table(pred,testdata[,"newp"])

#逻辑回归
library(glm)
m2<-glm(newp~.,data=traindata,family="binomial")
pred2<-ifelse(predict(m2,testdata,type="response")>0.2,"1","0")
table(pred2,testdata[,"newp"])  #0.279