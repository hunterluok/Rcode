
bisection<-function(ftn,x.l,x.r,tol=1e-9){
if(x.l>=x.r){
cat("error:x.l>=x.r\n")
return(NULL)
}

f.l<-ftn(x.l)
f.r<-ftn(x.r)

if(f.l==0){
return(x.1)
}else if(f.r==0){
return(x.r)
}
else if(f.l*f.r>0){
cat("error:ftn(x.l)*ftn(x.r)>0\n")
return(NULL)
}
n<-0
while((x.r-x.l)>tol){
x.m<-(x.l+x.r)/2
f.m<-ftn(x.m)
if(f.m==0){
return(x.m)
}
else if(f.l*f.m<0){
x.r<-x.m
f.r<-f.m
}else {
x.l<-x.m
f.l<-f.m
}
n<-n+1
cat("at iteration",n,"the root lies between",x.l,"and",x.r,"\n")
}
return((x.l+x.r)/2)
}
ftn5<-function(x) return(log(x)-exp(-x))
bisection(ftn5,1,2,tol=1e-06)



