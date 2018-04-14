
单变量元分析
library(metaSEM)

c1<-matrix(c(1,0.5,0.4,0.5,1,0.2,0.4,0.2,1),ncol=3,dimnames=list(c("x1","x2","x3"),c("x1","x2","x3")))
sdc1<-diag(c(1.2,1.3,1.4)) #标准离差

c2<-sdc1 %*% c1 %*% sdc1
c2
#样本协方差矩阵 

corsample<-asyCov(c2,n=50) #n为样本量

covsample<-asyCov(c2,n=50,cor.analysis=FALSE)

head(Jaramillo05)

summary(meta(y=r,v=r_v,data=Jaramillo05))


summary(meta(y=r,v=r_v,data=d1,intervals.type="LB"))

summary(meta(y=r,v=r_v,x=scale(IDV,scale=F),data=Jaramillo05))

#检验两个变量的影响是否是一致的。
m1<-meta(y=r,v=r_v,x=cbind(OC_alpha,JP_alpha),data=d1,model.name="Unequal coefficients")

constraint<-matrix(c("0*Slope_equal","0*Slope_equal"),nrow=1)
m2<-meta(y=r,v=r_v,x=cbind(OC_alpha,JP_alpha),data=d1,model.name="equal coefficients",coef.constraints=constraint)

分类变量的的调节效应检验

table(d1$Sales)
sales<-ifelse(d1$Sales=="sales",1,0)
nonsales<-ifelse(d1$Sales=="nonsales",1,0)
mixed<-ifelse(d1$Sales=="mixed",1,0)

intercept.constraint=matrix(0,ncol=1,nrow=1)
startvalues<-matrix(c("0*Slope1_1","0*Slope_2","0*Slope_3"),nrow=1,ncol=3)
startvalues

m3<-meta(y=r,v=r_v,x=cbind(sales,mixed,nonsales),data=d1,model.name="indicator coefficients",coef.constraints=startvalues,intercept.constraints=matrix(0,ncol=1,nrow=1))




