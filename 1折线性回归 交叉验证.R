
f<-function(df){
mean.sqr.errs<-sapply(1:nrow(df),fr,df)
list("mean sqr errs"=mean.sqr.errs,"overall_mean_sqr_err"=mean(mean.sqr.errs),
"std_dev_mean_sqrerr"=sd(mean.sqr.errs))
}

fr<-function(k,df){
mod<-lm(MEDV~.,data=df[-k,])
pred<-predict(mod,df[k,])
sqr.err<-(pred-df[k,"MEDV"])^2
mean(sqr.err)
}

res<-f(bh);res

res$mean_sqr_errs