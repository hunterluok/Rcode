

trf<-function(ftn,a,b,n){
h<-(b-a)/n
x.vec<-seq(a,b,by=h)
f.vec<-sapply(x.vec,ftn)
t<-h*(f.vec[1]/2+sum(f.vec[2:n])+f.vec[n+1]/2)
return(t)
}
ftn6<-function(x) return(4*x^3)

simpson<-function(ftn,a,b,n=100){
n<-max(c(2*(n%/%2),4))
h<-(b-a)/n
x.vec1<-seq(a+h,b-h,by=2*h)
x.vec2<-seq(a+2*h,b-2*h,by=2*h)
f.vec1<-sapply(x.vec1,ftn)
f.vec2<-sapply(x.vec2,ftn)
s<-h/3*(ftn(a)+ftn(b)+4*sum(f.vec1)+2*sum(f.vec2))
return(s)
}

ftn<-function(x) return(4*x^3)
simpson(ftn,0,1,20)