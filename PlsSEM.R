
imag<-c(0,0,0,0,0,0)  
expe<-c(1,0,0,0,0,0) #根据顺序 被指向则为1，否则为0
qual<-c(0,1,0,0,0,0)
val<-c(0,1,1,0,0,0)
sat<-c(1,1,1,1,0,0)
loy<-c(1,0,0,0,1,0)
sat.mat<-rbind(imag,expe,qual,val,sat,loy)  
sat.sets<-list(1:5,6:10,11:15,16:19,20:23,24:27) #对应的题项数，注意是List
sat.mod<-rep("A",6) #A代表反应型 B代表形成型。
res<-plspm(data,sat.mat,sat.sets,sat.mod,scaled=F)
summary(res)
plot(res,what="all")

