#求一个矩阵中的上三角阵
f<-function(xx){
data<-matrix(0,nrow(xx),ncol(xx))
for(i in 1:nrow(xx)){
for(j in 1:ncol(xx)){
if(i<=j)
data[i,j]<-xx[i,j]
}
}
return(data)
}