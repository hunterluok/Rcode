
x<-c(2,-4,5,-2,7,8,5,-10,3,-1)
f<-function(x){
n<-length(x)
m<-n/2
ls<-sum(x[1:m])
lss<-0
for(i in m:1){
lss<-lss+x[i]
if(lss>ls){
ls<-lss
maxl<-i
}
}

rs<-sum(x[(m+1):n])
rss<-0
for(i in (m+1):n){
rss<-rss+x[i]
if(rss>rs){
rs<-rss
maxr<-i
}
}
c(maxl,maxr,ls,rs,rs+ls,m)
}

f(x)


s<-c()
for(i in 4:2){
s<-c(s,x[i])
}
s





