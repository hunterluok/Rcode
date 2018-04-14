data(Hitters)
data<-na.omit(Hitters)
sa<-log(data$Salary) #取对数，默认为自然对数
data1<-data.frame(data,sa)
data<-data1[,-19]
head(data)
set.seed(1)
train<-sample(1:nrow(data),200)
traind<-data[train,]
testd<-data[-train,]
library(gbm)
set.seed(2)
mo1<-gbm(sa~.,traind,distribution="gaussian",n.trees=1000,interaction.depth=4,shrinkage=0.01)  #分类树则 distribution="bernoulli"
summary(mo1)  #查看变量的重要性。
mean(mo1$train.error) #训练误差率
pred1<-predict(mo1,traind,n.trees=1000)
mean((pred1-traind[,"sa"])^2)   #在测试集上的误差为0.378
pred<-predict(mo1,testd,n.trees=1000)
mean((pred-testd[,"sa"])^2)   #在测试集上的误差为0.325
#比较不同的树深度的 不同测试误差率
errcontian<-rep(0,10)
for(i in 1:10){
mok<-gbm(sa~.,traind,distribution="gaussian",n.trees=1000,interaction.depth=i) 
predk<-predict(mok,testd,n.trees=1000)
errcontian[i]<-mean((predk-testd[,"sa"])^2) 
}
errcontian  

#比较不同的压缩值)
set.seed(3)
xx<-seq(0,1,0.005)
err<-rep(0,length(xx))
for(i in 1:length(xx)){
mokk<-gbm(sa~.,traind,distribution="gaussian",n.trees=500,interaction.depth=4,shrinkage=xx[i])
predkk<-predict(mokk,testd,n.trees=500)
err[i]<-mean((predkk-testd[,"sa"])^2) 
}
pl
ot(xx,err)
err
#err[3]=0.1333952




