
rdacb.kfold.crossval.reg<-function(df,nfolds){
fold<-sample(1:nfolds,nrow(df),replace=T)
mean.sqr.errs<-sapply(1:nfolds,rdacb.kfold.crossval.reg.itr,df,fold)
list("mean_sqr_errs"=mean.sqr.errs,
"overall_mean_sqr_err"=mean(mean.sqr.errs),
"std_dev_mean_sqr_err"=sd(mean.sqr.errs))
}

rdacb.kfold.crossval.reg.itr<-function(k,df,fold){
trg.idx<-!fold %in% c(k)
test.idx<-fold %in% c(k)
mod<-lm(MEDV~.,data=df[trg.idx,])   #线性回归的K折 交叉认证

pred<-predict(mod,df[test.idx,])
sqr.errs<-(pred-df[test.idx,"MEDV"])^2
mean(sqr.errs)
}

res<-rdacb.kfold.crossval.reg(bh,5)
res$mean_sqr_errs
res$overall_mean_sqr_err
res$std_dev_mean_sqr_err