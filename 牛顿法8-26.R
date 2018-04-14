line.search<-function(f,x,y,tol=1e-9,a.max=2^5){

if(sum(abs(y))==0) return(x) 
g<-function(a) return(f(x+a*y))
a.l<-0
g.l<-g(a.l)
a.m<-1
g.m<-g(a.m)
while((g.m<g.1)&(a.m>tol)){
a.m<-a.m/2
g.m<-g(a.m)
}
if((a.m<=tol)&(g.m<g.l))return(x)
a.r<-2*a.m
g.r<-g(a.r)
while((g.m<g.r)&(a.r<a.max)){
a.m<-a.r
g.m<-g.r
a.r<-2*a.m
g.r<-g(a.r)
}
if((a.r>=a.max)&(g.m<g.r))return(x+a.max*y) vcc 
a<-gsetion(g,a.l,a.r,a.m)
return(x+a*y)
}

newton<-function(f3,x0,tol=1e-9,n.max=100){
x<-x0
f3.x<-f3(x)
n<-0
while((max(abs(f3.x[[2]]))>tol)&(n<n.max)){
x<-x-solve(f3.x[[3]],f3.x[[2]])
f3.x<-f3(x)
n<-n+1
}
if(n==n.max){
cat("newton failed to converge\n")
}else{
return(x)
}
}
f3<-function(x){
a<-x[1]^2/2-x[2]^2/4
b<-2*x[1]-exp(x[2])
f<-sin(a)*cos(b)
f1<-cos(a)*cos(b)*x[1]-sin(a)*sin(b)*2
f2<--cos(a)*cos(b)*x[2]/2+sin(a)*sin(b)*exp(x[2])
f11<--sin(a)*cos(b)*(4+x[1]^2)+cos(a)*cos(b)-cos(a)*sin(b)*4*x[1]
f12<-sin(a)*cos(b)*(x[1]*x[2]/2+2*exp(x[2]))+cos(a)*sin(b)*(x[1]*exp(x[2])+x[2])
f22<-sin(a)*cos(b)*(x[2]^2/4+exp(2*x[2]))-cos(a)*cos(b)/2-cos(a)*sin(b)*x[2]*exp(x[2])+sin(a)*sin(b)*exp(x[2])
return(list(f,c(f1,f2),matrix(c(f11,f12,f12,f22),2,2)))
}

for(x0 in seq(1.4,1.6,.1)){
for(y0 in seq(0.4,0.6,0.1)){
cat(c(x0,y0),'-->',newton(f3,c(x0,y0)),'\n')
}
}



















