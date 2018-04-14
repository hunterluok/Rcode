
ÆÓËØ±´Ò¶Ë¹
ppos<-sum(dtrain[,outcome]==pos)/length(dtrain[,outcome])
nbayes<-function(ppos,pf){
pneg<-1-ppos
smoothingepsilon<-1.0e-5
scorepos<-log(ppos+smoothingepsilon)+rowSums(log(pf/ppos+smoothingepsilon))
scoreneg<-log(pneg+smoothingepsilon)+rowSums(log((1-pf)/(1-ppos)+smoothingepsilon))
m<-pmax(scorepos,scoreneg)
expscorepos<-exp(scorepos-m)
expscoreneg<-exp(scoreneg-m)
expscorepos/(expsocrepos+expscoreneg)}

pvars<-paste('pred',c(numericvars,catvars),sep='')
dtrain$nbpredl<-nbayes(ppos,dtrain[,pvars])
dcal$nbpredl1<-nbayes(ppos,dcal[,pvars])
dtest$nbpred1<-nbayes(ppos,dtest[,pvars])
print(calcauc(dtrain$nbpred1,dtrain[,outcome]))

library(e1071)
lvars<-c(catvars,numericvars)
ff<-paste('as.factor(',outcome,'>0)~',paste(lvars,collapse='+'),sep='')

nbmodel<-naiveBayes(as.formula(ff),data=dtrain)
dtrain$nbpred<-predict(nbmodel,dtrain,type='raw')[,'TRUE']
dcal$nbpred<-predict(nbmodel,dcal,type='raw')[,'TRUE']
dtest$nbpred<-predict(nbmodel,dtest,type='raw')[,'TRUE']

calcauc(dtrain$nbpred,dtrain[,outcome])
calcauc(dtest$nbpred,dtest[,outcome])
calcauc(dcal$nbpred,dcal[,outcome])