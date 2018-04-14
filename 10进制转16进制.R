f<-function(n0){
t<-c()
z<-c()
fh<-n0
p<-c(1:9,letters[1:6])
while(fh>15){
x<-fh%%16
if(x>9){x<-p[x]}
t<-c(t,x)
fh<-fh%/%16
}
if(fh<=15 && fh>9){
fh<-p[fh]
}
t<-c(t,fh)
t<-rev(t)
for(i in 1:length(t)){
z<-paste(z,t[i],sep="")
}
cat(n0,"转化为16进制数为Ox",z,"\n",sep="")
}
f(175)




