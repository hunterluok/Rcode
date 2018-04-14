vars<-colnames(bank);vars

catvars<-vars[sapply(bank[,vars],class) %in% c('factor','character')]
catvars

for(i in 1:length(catvars)){
t[[length(catvars)+1]]<-list();t
t[[i]]<-table(bank[,catvars[i]])*100/dim(bank)[[1]]
}
t

sub<-sample(1:nrow(bank),round(nrow(bank)/4))
length(sub)
train<-bank[-sub,]
test<-bank[sub,]
dim(train);dim(test)

library(adabag)
library(rpart)

bag<-bagging(y~.,train,mfinal=5)

bag1<-bagging(y~.,train,mfinal=5,control=rpart.control(maxdepth=3))
sort(bag1$importance,decreasing=T)
sort(bag$importance,decreasing=T)

pre_bag<-predict(bag,test)
names(pre_bag)


for(i in 1:n){print("晚上好，以上是自动回复信息~Bye")}




