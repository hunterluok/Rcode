
fs<-function(ftn,x0,xmin=x0-1,xmax=x0+1){
x<-seq(xmin,xmax,(xmax-xmin)/200)
fx<-sapply(x,ftn)
plot(x,fx,type="l",xlab="x",ylab="f(x)",main="fixed point f(x)=x",col="blue",lwd=2)
lines(c(xmin,xmax),c(xmin,xmax),col="blue")

xold<-0
xnew<-ftn(xold)
lines(c(xold,xold,xnew),c(xold,xnew,xnew),col="red")
lines(c(xnew,xnew),c(xnew,0),lty=2,col="red")
cat("last x value",xnew," ")
continue<-readline("continue (y or n)?")=="y"
while(continue){
xold<-xnew
xnew<-ftn(xold)
lines(c(xold,xold,xnew),c(xold,xnew,xnew),col="red")
lines(c(xnew,xnew),c(xnew,0),lty=2,col="red")
cat("last x value ",xnew," ")
continue<-readline("continue(y or n)?")=="y"
}
return(xnew)
}

ftn<-function(x){
return()
}
fs(ftn,2)