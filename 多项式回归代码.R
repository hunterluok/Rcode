ployfit<-function(y,x,maxdeg){
pwrs<-powers(x,maxdeg)
pwrs<-list()
class(lmout)<-"polyreg"
for(i in 1:maxdeg){

lmo<-lm(y~pwrs[,1:i])
lmo$fitted.xvvalues<-lvoneout(y,pwrs[,1:i,drop=F])
lmout[[i]]<-lmo
}
lmout$x<-x
lmout$y<-y
return(lmout)
}

print.polyreg<-function(fits){
maxdeg<-length(fits)-2
n<-length(fits$y)
tbl<-matrix(nrow=maxdeg,ncol=1)
cat("mean squared prediction errors,by degree\n")
colnames(tbl)<-"MSPE"
for(i in 1:maxdeg){
fi<-fits[[i]]
errs<-fits$y-fi$fitted.xvvalues
spe<-sum(err^2)
tbl[i,1]<-spe/n
}
print(tbl)
}

plot.polyreg<-function(fits){
plot(fits$x,fits$y,xlab="x",ylab="y")
maxdg<-length(fits)-2
cols<-c("red","green","blue")
dg<-curvecount<-1
while(dg<maxdg){
prompt<-paste("RETURN for XV fit for gegree",dg,"or type degree","or q for quit")
rl<-readline(prompt)
dg<-if(r1=="") dg else if(r1!="q") as.integer(r1) else break
lines(fits$x,fits[[dg]]$fitted.values,col=cols[curvecount%%3+1])
dg<-dg+1
curvecount<-curvecount+1
}
}
powers<-function(x,dg){
pw<-matrix(x,nrow=length(x))
prod<-x
for(i in 2:dg){
prod<-prod*x
pw<-cbind(pw,prod)
}
return(pw)
}

lvoneout<-function(y,xmat){
n<-length(y)
predy<-vector(length=n)
for(i in 1:n){
lmo<-lm(y[-i]~xmat[-i,])
betahat<-as.vector(lmo$coef)
predy[i]<-betahat %*% c(1,xmat[i,])
}
return(predy)
}

poly<-function(x,cfs){
val<-cfs[1]
prod<-1
dg<-length(cfs)-1
for(i in 1:dg){
prod<-prod*x
val<-val+cfs[i+1]*prod
}
}

plot.poly<-function(fits){
plot(fits$x,fits$y,xlab="x",ylab="y")
maxdg<-length(fits)-2
cols<-c("red","green","blue")
dg<-curvecount<-1
while(dg<-maxdg){
prompt<-paste("return for xv fit for degree",dg,"or type degree","or q for quit")
r1<-readline(prompt)
dg<-if(r1=="")dg  else if(r1!="q") as.integer(r1) else break
lines(fits$x,fits[[dg]]$fitted.values,col=cols[curvecount%%3+1])
dg<-dg+1
curvecount<-courvecount+1
}
}



















