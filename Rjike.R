

filedatamodel<-function(data){
names(data)<-c("uid","iid","pref")
user<-unique(data$uid)

item<-unique(sort(data$iid))

uidx<-match(data$uid,user)
iidx<-match(data$iid,item)
M<-matrix(0,length(user),length(item))

i<-cbind(uidx,iidx,pref=data$pref)
for(n in 1:nrow(i)){
M[i[n,][1],i[n,][2]]<-i[n,][3]
}
dimnames(M)[[2]]<-item
M
}
filedatamodel(data)

euclideandistances<-function(M){
row<-nrow(M)
s<-matrix(0,nrow,nrow)
for(z1 in 1:nrow){
for(z2 in 1:nrow){
if(z1<z2){
num<-intersect(which(M[z1,]!=0,which(M[z2,]!=0))
sum<-0
for(z3 in num){
sum<-sum+(M[z1,][z3]-M[z2,][z3])^2
}
s[z2,z1]<-length(num)/(1+sqrt(sum))
if(s[z2,z1]>1) s[z2,z1]<-1
if(s[z2,z1]<-1)s[z2,z1]<--1
}
}
}
ts<-t(s)
w<-which(upper.tri(ts))
s[w]<-ts[w]
s
}
euclideandistances(M)
