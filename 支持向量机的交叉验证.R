library(ISLR)
library(e1071)
mo<-tune(svm,y~.,data=d,kernel="radial",ranges=list(cost=seq(0.1,5.1,0.3),gamma=c(0.1,1,2,3)))
t<-summary(mo)
best<-mo$best.model  #选择最优模型
pred<-predict(best,data=d)  
table(pred,d$y)
mean(pred!=d$y)  计算错误率

mo1<-svm(y~.,data=d,kernel="radial",cost=1,gamma=2) #这里不能用list
t<-summary(mo1)
plot(mo1,d,x1~x2)  #x1,x2 代表不同的变量
ct<-seq(0.1,5,0.5)
ga<-seq(0.5,5,1)
err<-matrix(0,nrow=length(ct),ncol=length(ga))
for(i in 1:length(ct)){
for(j in 1:length(ga)){
model<-svm(y~.,data=d,kernel="radial",cost=ct[i],gamma=ga[j])
pred<-predict(model,data=d)
err[i,j]<-mean(pred!=d$y)
}
}
err  #最优为0.00255
which.min(err) #求最小的那个误差值
ct<-seq(0.1,5,0.5)
de<-seq(1,5,1)
err<-matrix(0,nrow=length(ct),ncol=length(ga))
for(i in 1:length(ct)){
for(j in 1:length(ga)){
model<-svm(y~.,data=d,kernel="polynomial",cost=ct[i],degree=de[j])
pred<-predict(model,data=d)
err[i,j]<-mean(pred!=d$y)
}
}
err  #0.056  #画图见下
plot(err[1,],col=2,type="l")
for(i in 2:5){
points(err[i,],col=i+1,type="l")
}
