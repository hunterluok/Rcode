
data(OJ)
head(OJ)
set.seed(1)
train<-sample(1:nrow(OJ),800)

m1<-svm(Purchase~.,data=OJ[train,],cost=0.01,kernel="linear")
#测试集的错误率
pred<-predict(m1,OJ[-train,])
table(pred,OJ[-train,"Purchase"])
mean(pred!=OJ[-train,"Purchase"])
#训练集的错误率
predd<-predict(m1,OJ[train,])
table(predd,OJ[train,"Purchase"])
mean(predd!=OJ[train,"Purchase"])

#交叉验证得到的结果：
se<-seq(0.1,5,0.1)
m<-tune(svm,Purchase~.,data=OJ[train,],ranges=list(cost=se),kernel="radial")
m0<-m$best.model
#测试集的错误率
pred<-predict(m0,OJ[-train,])
table(pred,OJ[-train,"Purchase"])
mean(pred!=OJ[-train,"Purchase"])
#训练集的错误率
predd<-predict(m0,OJ[train,])
table(predd,OJ[train,"Purchase"])
mean(predd!=OJ[train,"Purchase"])
