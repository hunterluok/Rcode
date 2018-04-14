SVM例子：

library(kernlab)
library(ggplot2)
data(spirals)

sc<-specc(spirals,centers=2)
s<-data.frame(x=spirals[,1],y=spirals[,2],class=as.factor(sc))
ggplot(data=s)+geom_text(aes(x=x,y=y,label=class,color=class))+coord_fixed()+theme_bw()+
theme(legend.position='none')
set.seed(2335246L)
s$group<-sample.int(100,size=dim(s)[[1]],replace=T)
strain<-subset(s,group>10)
stest<-subset(s,group<=10)
msvm<-ksvm(class~.,strain,kernel="vanilladot")
stest$predsvm<-predict(msvm,stest,type="response")
ggplot()+geom_text(data=stest,aes(x=x,y=y,label=predsvm),size=12)+geom_text(data=s,aes(x=x,y=y,label=class,color=class),alpha=0.7)+
coord_fixed()+theme_bw()+theme(legend.position="none")
msvm<-ksvm(class~.,strain,kernel="rbfdot")
stest$predsvm<-predict(msvm,stest,type="response")
ggplot()+geom_text(data=stest,aes(x=x,y=y,label=predsvm),size=12)+geom_text(data=s,aes(x=x,y=y,label=class,color=class),alpha=0.7)+
coord_fixed()+theme_bw()+theme(legend.position="none")

逻辑回归：
spamd<-read.table("C:/Users/lenovo/Desktop/数据科学 理论、方法与R语言实践/zmPDSwR-master/Spambase/spamD.tsv",header=T,sep="\t")
spamtrain<-subset(spamd,spamd$rgroup>=10)
spamtest<-subset(spamd,spamd$rgroup<10)
spamvars<-setdiff(colnames(spamd),list('rgroup','spam'))
spamformula<-as.formula(paste('spam=="spam"',paste(spamvars,collapse='+'),sep='~'))
spammodel<-glm(spamformula,family=binomial(link='logit'),data=spamtrain)
spamtest$pred<-predict(spammodel,spamtest,type="response")
print(with(spamtest,table(y=spam,glpred=pred>0.5)))

spamformulav<-as.formula(paste('spam=="spam"',paste(spamvars,collapse='+'),sep='~'))
svmm<-ksvm(spamformulav,spamtrain,kernel='rbfdot',C=10,prob.model=T,cross=5,class.weights=c('spam'=1,'non-sapm'=10))
spamtest$svmpred<-predict(svmm,spamtest,type="response")
print(with(spamtest,table(spam,svmpred=svmpred>0.5)))

samecut<-sort(spamtest$pred)[length(spamtest$pred)-162]
print(with(spamtest,table(y=spam,glpred=pred>samecut)))
