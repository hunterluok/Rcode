KNN 受试者特征曲线
library(class)
nk<-200
knntrain<-dtrain[,selvars]
knnc1<-dtrain[,outcome]==pos
knnpred<-function(df){
knndecision<-knn(knntrain,df,knnc1,k=nk,prob=T)
ifelse(knndecision==TRUE,attributes(knndecision)$prob,
1-(attributes(knndecision)$prob))
}

print(calcauc(knnpred(dtrain[,selvars]),dtrain[,outcome]))
print(calcauc(knnpred(dtest[,selvars]),dtest[,outcome]))
print(calcauc(knnpred(dcal[,selvars]),dcal[,outcome]))


loglikelyhood<-function(outcol,predcol){
sum(ifelse(outcol==pos,log(predcol),log(1-predcol)))
}
selvars<-c()
minstep<-5
baseratecheck<-loglikelyhood(dcal[,outcome],sum(dcal[,outcome]==pos)/length(dcal[,outcome]))

for(v in catvars){
pi<-paste('pred',v,sep='')
licheck<-2*((loglikelyhood(dcal[,outcome],dcal[,pi])-baseratecheck))
if(licheck>minstep){
print(sprintf("%s,calibrationscore:%g",pi,licheck))
selvars<-c(selvars,pi)
}
}

for(v in numericvars){
pi<-paste('pred',v,sep='')
licheck<-2*((loglikelyhood(dcal[,outcome],dcal[,pi])-baseratecheck))
if(licheck>minstep){
print(sprintf("%s,calibrationscore:%g",pi,licheck))
selvars<-c(selvars,pi)
}
}

plotroc<-function(predcol,outcol){
perf<-performance(prediction(predcol,outcol==pos),'tpr','fpr')
pf<-data.frame(fpr=perf@x.values[[1]],
tpr=perf@y.values[[1]])

ggplot()+geom_line(data=pf,aes(x=fpr,y=tpr))+geom_line(aes(x=c(0,1),y=c(0,1)))
}
print(plotroc(knnpred(dtest[,selvars]),dtest[,outcome]))


