#逻辑回归的用法 二元
complications<-c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
riskfactors<-c("URF_DIAB","URF_CHYPER","URF_PHYPER","URF_ECLAM")
y<-"atRisk"
x<-c("PWGT","UPREVIS","CIG_REC","GESTREC3","DPLURAL",complications,riskfactors)
fmla<-paste(y,paste(x,collapse='+'),sep='~')
print(fmla)
model<-glm(fmla,train,family=binomial(link='logit'))
train$pred<-predict(model,train,type="response")
test$pred<-predict(model,test,type="response")
library(ggplot2)

ggplot(train,aes(x=trainp,color=atRisk,linetype=atRisk))+geom_density()\

library(ROCR)
library(grid)
predobj<-prediction(train$pred,train$atRisk)
precobj<-performance(predobj,measure="prec")
recobj<-performance(predobj,measure="rec")
precision<-(precobj@y.values)[[1]]
prec.x<-(precobj@x.values)[[1]]
recall<-(recobj@y.values)[[1]]
rocframe<-data.frame(threshold=prec.x,precision=precision,recall=recall)
nplot<-function(plist){
n<-length(plist)
grid.newpage()
pushViewport(viewport(layout=grid.layout(n,1)))
vplayout=function(x,y){viewport(layout.pos.row=x,layout.pos.col=y)}
for(i in 1:n){
print(plist[[i]],vp=vplayout(i,1))
}}
pnull<-mean(as.numeric(train$atRisk))
p1<-ggplot(rocframe,aes(x=threshold))+geom_line(aes(y=precision/pnull))+
coord_cartesian(xlim=c(0,0.05),ylim=c(0,10))
p2<-ggplot(rocframe,aes(x=threshold))+geom_line(aes(y=recall))+coord_cartesian(xlim=c(0,0.05))
nplot(list(p1,p2))

ctab.test<-table(pred=test$pred>0.02,atRisk=test$atRisk)
ctab.test

pred<-predict(model,train,type="response")
llcomponents<-function(y,py){
y*log(py)+(1-y)*log(1-py)
}
edev<-sign(as.numeric(train$atRisk)-pred)*sqrt(-2*llcomponents(as.numeric(train$atRisk),pred))
summary(edev)

#计算偏差
loglikelihood<-function(y,py){
sum(y*log(py)+(1-y)*log(1-py))}
pnull<-mean(as.numeric(train$atRisk))
null.dev<--2*loglikelihood(as.numeric(train$atRisk),pnull)
pnull
null.dev
model$null.deviance

pred<-predict(model,train,type="response")
resid.dev<--2*loglikelihood(as.numeric(train$atRisk),pred)
resid.dev
model$deviance

testy<-as.numeric(test$atRisk)
testpred<-predict(model,test,type="response")
pnull.test<-mean(testy)
null.dev.test<--2*loglikelihood(testy,pnull.test)
resid.dev.test<--2*loglikelihood(testy,testpred)
pnull.test
null.dev.test
resid.dev.test

#拟合结果的显著性
df.null<-dim(train)[[1]]-1
df.model<-dim(train)[[1]]-length(model$coefficients)
df.null
df.model
deldev<-null.dev-resid.dev
deldf<-df.null-df.model
p<-pchisq(deldev,deldf,lower.tail=F)
deldev;deldf;p

pr2<-1-(resid.dev/null.dev)
print(pr2)
pr2.test<-1-(resid.dev.test/null.dev.test)
print(pr2.test)

#AIC信息准则
aic<-loglikelihood(as.numeric(train$atRisk),pred)-length(model$coefficients)
aic










