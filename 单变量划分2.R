
数值型：
mkpredn<-function(outcol,varcol,appcol){
cuts<-unique(as.numeric(quantile(varcol,probs=seq(0,1,0.1),na.rm=T)))
varc<-cut(varcol,cuts)
appc<-cut(appcol,cuts)
mkpredc(outcol,varc,appc)
}

cuts<-unique(as.numeric(quantile(1:20,probs=seq(0,1,0.1),na.rm=T)))
cuts

for(v in numericvars){
pi<-paste('pred',v,sep='')
dtrain[,pi]<-mkpredn(dtrain[,outcome],dtrain[,v],dtrain[,v])
dtest[,pi]<-mkpredn(dtrain[,outcome],dtrain[,v],dtest[,v])
dcal[,pi]<-mkpredn(dtrain[,outcome],dtrain[,v],dcal[,v])
auctrain<-calcauc(dtrain[,pi],dtrain[,outcome])
if(auctrain>=0.55){
auccal<-calcauc(dcal[,pi],dcal[,outcome])
print(sprintf("%s,trainauc:%4.3f calibrationauc:%4.3f",pi,auctrain,auccal))
}
}
ggplot(data=dcal)+geom_density(aes(x=predVar126,color=as.factor(churn)))

var<-'Var217'
aucs<-rep(0,100)
for(rep in 1:length(aucs)){
useforcalrep<-rbinom(n=dim(dtrainall)[[1]],size=1,prob=0.1)>0
predrep<-mkpredc(dtrain[!useforcalrep,outcome],dtrainall[!useforcalrep,var],dtrain[useforcalrep,var])
aucs[rep]<-calcauc(predrep,dtrainall[useforcalrep,outcome])
}
mean(aucs)
sd(aucs)

#十折交差验证
fcross<-function(){
useforcalrep<-rbinom(n=dim(dtrainall)[[1]],size=1,prob=0.1)>0
predrep<-mkpredc(dtrainall[!useforcalrep,outcome],dtrainall[!useforcalrep,var],dtrainall[useforcalrep,var])
calcauc(predrep,dtrainall[useforcalrep,outcome])
}
aucs<-replicate(100,fcross())






