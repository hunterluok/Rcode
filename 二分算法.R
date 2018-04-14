二分算法：
f<-function(ftn,xl,xr,tol=1e-9){
if(xl>xr){
cat("xl不能大于xr")
return(NULL)
}
fl<-ftn(xl)
fr<-ftn(xr)
if(fl==0){
return(xl)
}
else if(fr==0){
retrun(xr)
}else if(fr*fl>0)
{
cat("没法计算") 
return(NULL)
}
n<-0
while((xr-xl)>tol){
xm<-(xr+xl)/2
fm<-ftn(xm)
if(fm==0){
return(xm) 
}
else if(fm*fl<0){
xr<-xm
fr<-fm
}
else{
xl<-xm
fl<-fm
}
n<-n+1
cat("at iteration",n,"the root liest between",xl,"and",xr,"\n")
}
return((xl+xr)/2)
}
ftn<-function(x) return(log(x)-exp(-x))
f(ftn,1,2,tol=1e-6)

不动点迭代算法
f1<-function(ftn,x0,tol=1e-9,iter.max=100){   #x(n+1)=f(x(n))
xold<-x0
xnew<-ftn(xold)
iter<-1
cat("ddd")

while((abs(xnew-xold)>tol)&&iter<iter.max){
xold<-xnew
xnew<-ftn(xold)
iter<-iter+1
cat("at iteration ",iter,"value of x is:",xnew,"\n")
}
if(abs(xnew-xold)>tol){
cat("算法没法收敛")
return(NULL)
}else{
cat("算法收敛")
return(xnew)
}
}
ftn<-function(x) return(exp(exp(-x)))
f1(ftn,2,tol=1e-6)

ftn1<-function(x) return(x-cos(x))
f1(ftn1,0,tol=1e-1)

ftn2<-function(x) {
fx<-cos(x)-x
dfx<--sin(x)-1
return(c(fx,dfx))
}
f(ftn2,0,tol=1e-9,iter.max=100)




