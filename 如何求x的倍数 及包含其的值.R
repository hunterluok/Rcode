
x<-c(1:100)
f<-function(x,N){
n1<-N
n2<-as.character(N)
x1<-c()
s1<-c()
for(i in 1:length(x)){
if(x[i]%% n1==0){
x1<-c(x1,x[i])
}
}                             #得到属于x但不属于x1的数 进行后面的
x2<-x[!x%in% x1]  
x2<-as.character(x2)
for(i in 1:length(x2)){
for(j in 1:nchar(x2[i])){
if(substr(x2[i],j,j)==n2){
s1<-c(s1,x2[i])
}
}
}
s2<-c(x1,s1)
s3<-sort(as.numeric(s2))
return(list(s3,x1,s1,length(s3)))
}
f(x,2)

