library(tree)
library(ISLR)
attach(Carseats)
high=ifsels(Sales<8,"no","yes")
newdata<-=data.frame(Carseats,high)

#建模型
set.seed(2)
train=sample(1:nrow(d),200)
dtest=d[-train,]
htest=high[-train]

m2<-tree(high~.-Sales,d,subset=train)
pred<-predict(m2,dtest,type="class")
table(pred,htest)

#交叉验证选择最优的模型节点数目
set.seed(3)
cvm<-cv.tree(m2,FUN=prune.misclass)#分类错误率法
names(cvm)
par(mfrow=c(2,2))
plot(cvm$size,cvm$dev,type="b")
plot(cvm$k,cvm$dev,type="b")

#对最优模型进行查看
pm=prune.misclass(m2,best=9)
plot(pm);text(pm,pretty=0)
pred1=predict(pm,dtest,type="class")
table(pred1,htest)






