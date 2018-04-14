#回归树构建
library(MASS)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
mm=tree(medv~.,Boston,subset=train)

summary(mm)

m1<-cv.tree(mm)
plot(m1$size,m1$dev,type="b")
prunemm<-prune.tree(mm,best=5)
plot(prunemm);text(prunemm,pretty=0)

yhat=predict(mm,newdata=Boston[-train,])
test=Boston[-train,"medv"]
plot(yhat,test)
abline(0,1)
mean((yhat-test)^2)

#bagging 和随机森林

install.packages("randomForest")
library(randomForest)
mb=randomForest(medv~.,Boston,subset=train,mtry=13,importanct=TRUE)
mb

#装袋法
bagmodel<-randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat<-predict(bagmodel,newdata=Boston[-train,])
mean((yhat-test)^2)

#随机森林
set.seed(1)
rfmodel=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yrf=predict(rfmodel,Boston[-train,])
mean((yrf-test)^2)

#提升法
install.packages("gbm")
library(gbm)
mt<-gbm(medv~.,Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(mt)

par(mfrow=c(1,2))
plot(mt,i="rm")
plot(mt,i="lstat")

#预测
yts=predict(mt,Boston[-train,],n.trees=5000)
mean((yts-test)^2)


mt1<-gbm(medv~.,Boston[train,],distribution="gaussian",n.trees=5000,
shrinkage=0.2,interaction.depth=4)
summary(mt1)

yts1=predict(mt1,Boston[-train,],n.trees=5000)
mean((yts1-test)^2)




