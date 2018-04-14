#如何差别 变量的类型# 单变量模型#
catvars<-vars[sapply(dtrainall[,vars],class) %in% c('factor','character')]
numericvars<-vars[sapply(dtrainall[,vars],class) %in% c('numeric','integer')]
useforcal<-rbinom(n=dim(dtrainall)[[1]],size=1,prob=0.1)>0  #这个地方需要注意
dcal<-subset(dtrainall,useforcal)
dtrain<-subset(dtrainall,!useforcal)
table218<-table(var218=dtrain[,'Var218'],churn=dtrain[,outcome],useNA="ifany")
print(table218)
print(table218[,2]/(table218[,1]+table218[,2]))

mkpredc<-function(outcol,varcol,appcol){
ppos<-sum(outcol==pos)/length(outcol)
natab<-table(as.factor(outcol[is.na(varcol)]))
pposwna<-(natab/sum(natab))[pos]
vtab<-table(as.factor(outcol),varcol)
pposwv<-(vtab[pos,]+1e-3*ppos)/(colSums(vtab)+1.0e-3)
pred<-pposwv[appcol]
pred[is.na(appcol)]<-pposwna
pred[is.na(pred)]<-ppos
pred
}

for(v in catvars){
pi<-paste('pred',v,sep='')
dtrain[,pi]<-mkpredc(dtrain[,outcome],dtrain[,v],dtrain[,v])
dcal[,pi]<-mkpredc(dtrain[,outcome],dtrain[,v],dcal[,v])
dtest[,pi]<-mkpredc(dtrain[,outcome],dtrain[,v],dtest[,v])
}
library('ROCR')
calcauc<-function(predcol,outcol){
perf<-performance(prediction(predcol,outcol==pos),'auc')
as.numeric(perf@y.values)
}
for(v in catvars){
pi<-paste('pred',v,sep='')
auctrain<-calcauc(dtrain[,pi],dtrain[,outcome])
if(auctrain>=0.8){ auccal<-calcauc(dcal[,pi],dcal[outcome])
print(sprintf("%s,trainAUC:%4.3f calibrationgAUC:%4.3f",pi,auctrain,auccal))
 }
}
