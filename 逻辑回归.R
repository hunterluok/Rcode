train_errors<-vector("numeric",length=length(lambda))
var_errors<-vector("numeric",length=length(lambda))

for(i in 1:length(lambda)){
gt_rlm<-lm.ridge(y~.,data=gtrain,lambda=lambda[i])
gt_rlm_coef<-coef(gt_rlm)
predicted_train<-vector("numeric",length=nrow(gtrain))
for(j in 1:nrow(gtrain))
           {
predicted_train[j]<-gt_rlm_coef[1]+sum(gt_rlm_coef[-1]*gtrain[j,-1])
           }
train_errors[i]<-sum((predicted_train-gtrain[,1])^2,na.rm=TRUE)
predicted<-vector("numeric",length=nrow(gvalidata))

for(j in 1:length(gvalidata)) {
          predicted[j]<-gt_rlm_coef[1]+sum(gt_rlm_coef[-1]*gvalidata[j,-1])
                             }
var_errors[i]<-sum(predicted-gvalidata[,1]^2)
}

plot(lambda,var_errors,"l",col="red",xlab=expression(lambda),ylab="Training and validation errors",ylim=c(0,600))
points(lambda,train_errors,"l",col="green")
legend(6,500,c("training errors","validation errors"),col=c("green","red"),pch="-")

CVlm(df=VD,form.lm=formula(Voltage_Drop~Time+I(Time^2)+I(Time^3)+I(((Time-6.5)^3)*(sign(Time-6.5)==1))+I(((Time-13)^3)*(sign(Time-13)==1))),m=10,plotit="Observed")

lm_of<-lm.ridge(Y~poly(X,3),data=as.data.frame(OF),lambda=c(0,0.5,1,1.5,2,5,10,30))
lm_of$GCV

lm_gt<-lm.ridge(y~.,data=gtrain,lambda=seq(0,10,0.2))
lm_gt$GCV


