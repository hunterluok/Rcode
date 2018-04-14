
最大间隔分类器。
model<-svm(y~.,data=data,kernel="linear",cost=0.1,scale=FALSE)
plot(model,data)
model$index #查看在支持向量

#构造一组数据
xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdata<-data.frame(x=xtest,y=as.factor(ytest))

set.seed(1)
m11<-tune(svm,y~.,data=data,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(m11)
best<-m11$best.model #寻找最优模型 利用cost
summary(best)

ypred<-predict(best,testdata)
table(ypred,truth=testdata$y) 预

支持向量机
set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(-1,50))
data=data.frame(x=x,y=as.factor(y))
plot(x,col=y+3)

train=sample(200,100)

m1<-svm(y~.,data=data[train,],kernel="radial",gamma=1,cost=1)
plot(m1,data)
#改变cost的值得到其他模型

set.seed(1)
#高斯核函数。
mod<-tune(svm,y~.,data=data[train,],kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(mod)
best<-mod$best.model
table(data[-train,"y"],pred=predict(best,data[-train,]))

#多项式核函数。
mod1<-tune(svm,y~.,data=data[train,],kernel="polynomial",ranges=list(cost=c(0.1,1,10,100,1000),degree=c(1,2,3,4,5)))
summary(mod1)
best1<-mod$best.model
table(data[-train,"y"],pred=predict(best1,data[-train,]))

得到拟合值：
moo<-svm(y~.,data[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
fitted<-attributes(predict(moo,data[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,data[train,"y"],main="training data")

library(ROCR)
rocplot<-function(pred,truth,...){
predob<-prediction(pred,truth)
perf<-performance(predob,"tpr","fpr")
plot(perf,...)
}











