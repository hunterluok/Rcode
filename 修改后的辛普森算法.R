simpon_n<-function(ftn,a,b,n=100){
n<-max(c(2*(n %/% 2),4))
h<-(b-a)/n
x.vec1<-seq(a+h,b-h,by=2*h)
x.vec2<-seq(a+2*h,b-2*h,by=2*h)
f.vec1<-sapply(x.vec1,ftn)
f.vec2<-sapply(x.vec2,ftn)
s<-h/3*(ftn(a)+ftn(b)+4*sum(f.vec1)+2*sum(f.vec2))
return(s)
}
ftn<-function(x) return(4*x^3)
simpon_n(ftn,0,1,20)

phi<-function(x) return(exp(-x^2/2)/sqrt(2*pi))
Phi<-function(z){
if(z<0){
return(0.5-simpon_n(phi,z,0))
}else{
return(0.5+simpon_n(phi,0,z))
}
}
z<-seq(-5,5,by=0.1)
phi.z<-sapply(z,phi)
Phi.z<-sapply(z,Phi)
plot(z,Phi.z,type="l",ylab="",main="phi(z) and Phi(z)")
lines(z,phi.z)

ftn<-function(x) return(1/x)
s<-function(n) simpon_n(ftn,0.01,1,n)

n.vec<-seq(10,1000,by=10)
s.vec<-sapply(n.vec,s)

opar<-par(mfrow=c(1,2),pty="s",mar=c(4,4,2,1),las=1)
plot(n.vec,s.vec+log(0.01),type="l",xlab="n",ylab="error")
plot(log(n.vec),log(s.vec+log(0.01)),type="l",xlab="log(n)",ylab="log(error)")

修改后的辛普森算法：
simpon<-function(ftn,a,b,tol=1e-8,verbose=FALSE){
n<-4
h<-(b-a)/4
fx<-sapply(seq(a,b,by=h),ftn)
S<-sum(fx*c(1,4,2,4,1))*h/3
S.diff<-tol+1
while(S.diff>tol){
s.old<-S
n<-2*n
h<-h/2
fx[seq(1,n+1,by=2)]<-fx
fx[seq(2,n,by=2)]<-sapply(seq(a+h,b-h,by=2*h),ftn)
S<-h/3*(fx[1]+fx[n+1]+4*sum(fx[seq(2,n,by=2)])+2*sum(fx[seq(3,n-1,by=2)]))
S.diff<-abs(S-s.old)
}
if(verbose) cat("partition size",n,"\n")
return(S)
}

























