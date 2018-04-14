library(ISLR)
data(OJ)
train<-sample(1:nrow(OJ),800)
library(tree)
m1<-tree(Purchase~.,data=OJ,subset=train)
#训练集的错误率
predtrain<-predict(m1,OJ[train,],type="class")
table(predtrain,OJ[train,]$Purchase)
mean(predtrain!=OJ[train,]$Purchase) #训练集的错误率 0.158
#关于测试集的问题。
pred<-predict(m1,target,type="class")
target<-OJ[-train,]
table(pred,target$Purchase)
mean(pred!=target$Purchase) #测试集的错误率  0.2

#利用分类错误率来优化模型
set.seed(2)
m2<-cv.tree(m1,FUN=prune.misclass) #这个函数注意
plot(m2$dev,m2$size)
#最优为6个节点
prunemodel<-prune.misclass(m1,best=6)   #这一步记住

predtrainp<-predict(prunemodel,OJ[train,],type="class")
table(predtrainp,OJ[train,]$Purchase)
mean(predtrainp!=OJ[train,]$Purchase) #0.175
#在测试集上的错误率
predpp<-predict(prunemodel,target,type="class")
table(predpp,target$Purchase)
mean(predpp!=target$Purchase)   #0.15


pred11<-predict(m1,target) #得到的结果为 某一类的概率值。 
# 利用概率计算，而不会 "class"
f<-function(data){
name<-colnames(data)
n<-nrow(data)
d1<-rep(0,n)
for(i in 1:n){
d1[i]<-name[which.max(pred11[i,])]
}
d1
}
f(pred11)


