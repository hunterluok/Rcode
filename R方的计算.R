ggplot(data=dtest,aes(x=predlogpincp,y=predlogpincp-log(PINCP,base=10)))+
geom_point(alpha=0.2,color="black")+
geom_smooth(aes(x=predlogpincp,y=predlogpincp-log(PINCP,base=10)),color="black")

#R���ļ���

rsq<-function(y,f){1-sum((y-f)^2)/sum((y-mean(y))^2)}
rsq(log(dtrain$PINCP,base=10),predict(model,dtrain))
rsq(log(dtest$PINCP,base=10),predict(model,dtest))

rmse<-function(y,f) {sqrt( mean((y-f)^2))}
rmse(log(dtrain$PINCP,base=10),predict(model,dtrain))
rmse(log(dtest$PINCP,base=10),predict(model,dtest))

#�߼��ع�2
����ʵ��/zmPDSwR-master/CDC/natal2010Sample.tsv.gz",header=TRUE,sep="\t")