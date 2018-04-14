

牛顿拉夫森算法

f<-function(ftn,x0,iter.max=100,tol=1e-9){
x<-x0
fx<-ftn(x)
iter<-0

while( (abs(fx[1])>tol)&&(iter<iter.max)){
x<-x-fx[1]/fx[2]
fx<-ftn(x)
iter<-iter+1
cat("at iteration",iter,"value if x is:",x,"\n")
}
if(abs(fx[1])>tol){
cat("算法没法收敛")
return(NULL)
}else{
cat("算法收敛")
return(x)
}
}