
imag<-c(0,0,0,0,0,0)  
expe<-c(1,0,0,0,0,0) #����˳�� ��ָ����Ϊ1������Ϊ0
qual<-c(0,1,0,0,0,0)
val<-c(0,1,1,0,0,0)
sat<-c(1,1,1,1,0,0)
loy<-c(1,0,0,0,1,0)
sat.mat<-rbind(imag,expe,qual,val,sat,loy)  
sat.sets<-list(1:5,6:10,11:15,16:19,20:23,24:27) #��Ӧ����������ע����List
sat.mod<-rep("A",6) #A������Ӧ�� B�����γ��͡�
res<-plspm(data,sat.mat,sat.sets,sat.mod,scaled=F)
summary(res)
plot(res,what="all")
