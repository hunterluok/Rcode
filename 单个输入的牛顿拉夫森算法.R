new<-function(ftn,x0,xmin=x0-1,xmax=x0+1){
x<-seq(xmin,xmax,(xmax-xmin)/200)
fx<-c()
for(i in 1:length(x)){
fx[i]<-ftn(x[i])[1]
}
plot(x,fx,type="l",col="blue",lwd=2)
lines(c(xmin,xmax),c(0,0),col="blue")
xold<-x0
f.xold<-ftn(xold)
xnew<-xold-f.xold[1]/f.xold[2]
lines(c(xold,xold,xnew),c(0,f.xold[1],0),col="red")
cat("last x value ",xnew," ")
continue<-readline("continue (y or n)?")=="y"
while(continue){
xold<-xnew
f.xold<-ftn(xold)
xnew<-xold-f.xold[1]/f.xold[2]
lines(c(xold,xold,xnew),c(0,f.xold[1],0),col="red")
cat("last x value",xnew," ")
continue<-readline("continue (y or n)?")=="y"
}
return(xnew)
}

ftn<-function(x){
c1<-x^3-3
c2<-2*x^2
return(c(c1,c2))
}
new(ftn,1)
