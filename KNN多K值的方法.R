idx<-createDataPartition(educ$expense,p=0.7,list=FALSE)

trg<-educ[idx,]
val<-educ[-idx,]
res1<-knn.reg(trg[,7:12],test=NULL,y=trg[,6],k=2,algorithm="brute")

rmse<-sqrt(mean((res1$residuals)^2))

rdacb.knn.reg<-function(trg_predictors,val_predictors,trg_target,val_target,k){
library(FNN)
res<-knn.reg(trg_predictors,val_predictors,trg_target,k,algorithm="brute")
errors<-res$pred-val_target
rmse<-sqrt(sum(errors^2)/nrow(val_predictors))    #这个地方的求法 
cat(paste("RMSE for k=",toString(k),":",sep=" "),rmse,"\n")
rmse
}

rdacb.knn.reg(train[,7:12],val[,7:12],train[,6],val[,6],1)
rdacb.knn.reg(train[,7:12],val[,7:12],train[,6],val[,6],2)
rdacb.knn.reg(train[,7:12],val[,7:12],train[,6],val[,6],3)
rdacb.knn.reg(train[,7:12],val[,7:12],train[,6],val[,6],4)


rdacb.knn.reg.multi<-function(trg_predictors,val_predictors,trg_target,val_target,start_k,end_k){
rms_errors<-vector()  #这个地方注意
for(k in start_k:end_k){
rms_error<-rdacb.knn.reg(trg_predictors,val_predictors,trg_target,val_target,k)
rms_errors<-c(rms_errors,rms_error)  #这个地方的用法
}
plot(rms_errors,type="o",xlab="k",ylab="RMSE")
}
rdacb.knn.reg.multi(train[,7:12],val[,7:12],train[,6],val[,6],1,22)

