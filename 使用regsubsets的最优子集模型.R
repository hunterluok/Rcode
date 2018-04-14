par(mfrow=c(2,2))
plot(t$rss)
plot(t$adjr2,type="l")
mo1<-regsubsets(Salary~.,d,nvmax=19,method="forward")

#这里的用法需要多领会
for(i in 1:19){
coefi=coef(mo1,id=i)
pred=test.mat[,names(coefi)]%*% coefi
valerr[i]=mean((d$Salary[test]-pred)^2)
}

#构造基础的东西
k=10
set.seed(1)
folds<-sample(1:k,nrow(d),replace=T)
cv.errors<-matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

#进行10折交叉验证
for(j in 1:k){
mo=regsubsets(Salary~.,d[folds!=j,],nvmax=19)
for(i in 1:19){
pred<-predict(mo,d[folds==j,],id=i)
cv.errors[j,i]=mean((d$Salary[folds==j]-pred)^2)
}
}

#创建 适用于 regsubsets的 predict 函数
predict<-function(object,newdata,id,...){
form=as.formula(object$call[[2]])
mat=model.matrix(form,newdata)
coefi=coef(object,id=id)
xvars=names(coefi)
mat[,xvars]%*%coefi
}
