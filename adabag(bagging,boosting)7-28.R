data<-read.csv("F:/数据分析资料/数据挖掘-R语言实战/bank.csv",header=TRUE)
head(data)
sub<-sample(1:nrow(data),round(1/4*nrow(data)))
length(sub)
dtrain<-data[-sub,]
dtest<-data[sub,]
dim(dtrain);dim(dtest)
dim(dtrain)[1]+dim(dtest)[1]==dim(data)[1]
library(adabag)
library(rpart)
bag<-bagging(y~.,dtrain,mfinal=5) #mfinal在迭代中生成5可树。
names(bag)
bag$votes
bag$calss[105:115]
sort(bag$importance,decreasing=TRUE)[1:5]
bag1<-bagging(y~.,data=dtrain,mfinal=5,control=rpart.control(maxdepth=3))#control 控制函数
pre_bag<-predict(bag,dtest)
names(pre_bag)
t<-table(pre_bag$class,dtest$y) #class是类别 
pre_bag$confusion
pre_bag$error # 注意错误率。
sub_minor=which(dtest=="yes")
sub_major=which(dtest=="no")
length(sub_minor);length(sub_major)
eminorb<-sum(pre_bag$class[sub_minor]!=dtest$y[sub_minor])/length(sub_minor)
emajorb<-sum(pre_bag$class[sub_major]!=dtest$y[sub_major])/length(sub_major)
eminorb
emajorb #这里计算有问题、

ADABOOT
boo<-boosting(y~.,data=dtrain,mfinal=5)
pre_boo<-predict(boo,dtest)
err_boo<-sum(pre_boo$class!=dtest$y)/nrow(dtest)eminorb<-sum(pre_boo$class[sub_major]!=dtest$y[sub_major])/length(sub_major)
emajorb<-sum(pre_boo$class[sub_major]!=dtest$y[sub_major])/length(sub_major)
eminorb<-sum(pre_boo$class[sub_minor]!=dtest$y[sub_minor])/length(sub_minor)