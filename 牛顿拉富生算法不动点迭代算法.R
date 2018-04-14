

fixedpoint<-function(ftn,x0,tol=1e-9,max.iter=100){
xold<-x0
xnew<-ftn(xold)
iter<-1
cat("at interation 1 value of x is :",xnew,"\n")

while((abs(xnew-xold)>tol)&&(iter<max.iter)){
xold<-xnew
xnew<-ftn(xold)
iter<-iter+1             #算法的核心部分
cat("at iteration ",iter,"value of x is:",xnew,"\n")
}

if(abs(xnew-xold)>tol){
cat("algotithm failed to converge\n")
return(NULL)
}else{
cat("algorithm converge\n")
return(xnew)
}}

ftn1<-function(x) return(exp(exp(-x)))
fixedpoint(ftn1,2)
ftn2<-function(x) return(x-log(x)+exp(-x))
fixedpoint(ftn2,2)
ftn3<-function(x) return(x+log(x)-exp(-x))
fixedpoint(ftn3,2)

newtonson<-function(ftn,x0,tol=1e-9,max.iter=100){
x<-x0
fx<-ftn(x)
iter<-0
while((abs(fx[1])<tol)&&(iter<max.iter)){
x<-x-fx[1]/fx[2]
fx<-ftn(x)
iter<-iter+1
cat("At iteration ",iter,"value if x is:",x,"\n")
}
if(abs(fx[1])>tol){
cat("algorithm failed to converge\n")
return(NULL)
}else{
cat("algoritn converged\n")
return(x)
}
}
ftn4<-function(x){
fx<-log(x)-exp(-x)
dfx<-1/x+exp(-x)
return(c(fx,dfx))
}
newtonson(ftn4,2)














