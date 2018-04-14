
回归树选择节点：
f<-function(x,y){

xx<-unique(x,decreasing=T)
ss<-numeric(length(xx)-1)

for(i in 1:length(ss)){
yr<-y[x>xx[i]]
yl<-y[x<=xx[i]]
rss<- sum((yr-mean(yr))^2)
lss<-sum((yl-mean(yl))^2)
ss[i]<-rss+lss
}
return(list(xnode=xx[which(ss==min(ss,na.rm=T))],minss=min(ss,na.rm=T),ss,xx))
}

f(cpus$cach,log10(cpus$perf))