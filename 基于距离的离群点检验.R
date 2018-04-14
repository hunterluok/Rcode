x<-c(1:6,12,14)
y<-c(2:7,13,18)
plot(x,y)
t<-data.frame(x,y)
t
d<-function(t,d,k){
s<-matrix(0,nrow(t),nrow(t))
co<-c()
for(i in 1:nrow(t)){
count<-0
for(j in 1:nrow(t)){
s[i,j]<-(sum((t[i,]-t[j,])^2))^(1/2)
if(s[i,j]<d)
count<-count+1
}
if(count<k){
co[i]<-1 
}else{
co[i]<--1
}
}
for(i in 1:length(co)){
if(co[i]>0){
cat("d[",i,"]","ОЄАлИєµг\n")
}
}
}
d(t,10,6)


