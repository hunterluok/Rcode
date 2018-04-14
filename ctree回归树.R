library(ISLR)
data(Carseats)
library(tree)
set.seed(1)
train<-sample(1:nrow(Carseats),2/3*nrow(Carseats))

m1<-tree(Sales~.,data=Carseats,subset=train) 
m1
t<-summary(m1)
names(t)
t$size

#在训练集上进行预测
pred<-predict(m1,Carseats[-train,])
err<-mean((pred-Carseats[-train,"Sales"])^2)
plot(m1);text(m1,pretty=0)

#对数进行 减去枝叶
cvm1<-cv.tree(m1)
plot(cvm1$size,cvm1$dev,type="b")

prunem<-prune.tree(m1,best=8)  可以用循环来选择更优的模型
plot(prunem)
text(prunem,pretty=0)

pred<-predict(prunem,Carseats[-train,])
err<-mean((pred-Carseats[-train,"Sales"])^2)
err
plot(prunem);text(prunem,pretty=0)
