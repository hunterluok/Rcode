for(i in 1:10){
 fit<-svm(churn~.,churnTrain[ind!=i,])
 predictions=predict(fit,churnTrain[ind==i,!names(churnTrain) %in% c("churn")])
 cor<-sum(predictions==churnTrain[ind==i,c("churn")])
 ac<-append(cor/nrow(churnTrain[ind==i,]),ac)
 }


ac<-c()
for(i in 1:10){
fit<-naiveBayes(churn~.,churnTrain[ind!=i,])
predictions<-predict(fit,churnTrain[ind==i,!names(churnTrain) %in% c("churn")])
cor<-sum(predictions==churnTrain[ind==i,c("churn")])
ac<-append(cor/nrow(churnTrain[ind==i,]),ac)
}
mean(ac)

tuned<-tune.svm(churn~.,data=trainset,gamma=10^-2,cost=10^2,tunecontrol=tune.control(cross=10))
summary(tuned)
ind<-sample(2,nrow(churnTrain),prob=c(0.7,0.3),replace=T)
trainset<-churnTrain[ind==1,]
testset<-churnTrain[ind==2,]
