
#选择数据子集


psub<-subset(dpus,with(dpus,(PINCP>1000)&(ESR==1)&(PINCP<=250000)&(PERNP>1000)&(PERNP<=250000)&(WKHP>=40)&(AGEP>=20)&(AGEP<=50)&(PWGTP1>0)&(COW %in% (1:7))&(SCHL %in% (1:24)))


data<-matrix(c(rep(2,3),rep(1,2),rep(3,5)),ncol=2)
colnames(data)<-c("x1","x2")
> map<-c("nan","nv")
> data<-as.data.frame(data)
> class(data)
[1] "data.frame"
> data$x1<-as.factor(map[data$x1])  #这句话是关键。#
> data
   x1 x2
1 nan  3
2 nan  3
3  nv  3
4  nv  3
5  nv  3
5  nv  3
