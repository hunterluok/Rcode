f<-function(dat,cols){
nms<-names(dat)
for( col in cols){
name<-paste0(nms[col],"_z")  #如何给变量命名的方法很重要
dat[name]<-scale(dat[,col])
}
cat(paste("scaled",length(cols),"variable(s)\n"))
dat
}

auto2<-apply(auto[,2:7],2,function(x) scale(x))

set.seed()
fit<-kmeans(auto[,10:15],5)

pairs(auto[,2:7],col=c(1:5)[fit$cluster])
library(cluster)
clusplot(auto1[,10:15],fit$cluster,color=TRUE,shade=TRUE,labels=0,lines=0) 画图；


f<-function(data,num_clust=15,seed=9876){
set.seed(seed)
ss<-numeric(num_clust)
ss[1]<-(nrow(data)-1)*sum(apply(data,2,var))
for(i in 2:num_clust){
ss[i]<-sum(kmeans(data,centers=i)$withinss)
}
plot(1:num_clust,ss,type="b",pch=18,xlab="#cluster",ylab="totall within_ss across clusters")
}

f1<-function(data,num_clust=15,seed=9876){
set.seed(seed)
ss<-numeric(num_clust)
ss[1]<-(nrow(data)-1)*sum(apply(data,2,var))
for(i in 2:num_clust){
ss[i]<-(kmeans(data,centers=i))$tot.withinss
}
plot(1:num_clust,ss,type="b",pch=18,xlab="#cluster",ylab="totall within_ss across clusters")
}
f(auto1[,10:15],15)