
x<-c(63,4,6,55,24,7,3,15,1)
f<-function(x){
n<-length(x)
for(i in 1:(n-1)){    #n-1 改为n 不影响结果 但是 考虑到n-i+1 至少要为2的话 会好点
index=1
for(j in 1:(n-i+1)){
if(x[index]<x[j]){
index<-j
}
temp<-x[n-i+1]
x[n-i+1]<-x[index]
x[index]<-temp
}
}
x
}

f(x)
