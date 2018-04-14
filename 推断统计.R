binom_ci<-function(x,n,alpha){
phat=x/n
ll<-phat-qnorm(alpha/2,lower.tail=FALSE)*sqrt(phat*(1-phat)/n)
ul<-phat+qnorm(alpha/2,lower.tail=FALSE)*sqrt(phat*(1-phat)/n)
return(paste("The",100*(1-alpha),"%Confidence Interval for Binomial Proportion is(",round(ll,4),",",round(ul,4),")",sep=''))
                           }


binom_ci_ksd<-function(x,sigma,alpha){
xbar=mean(x)
ll<-xbar-qnorm(alpha/2,lower.tail=FALSE)*sigma/sqrt(n)
ul<-xbar+qnorm(alpha/2,lower.tail=FALSE)*sigma/sqrt(n)
return(paste("The",100*(1-alpha),"%Confidence Interval for Binomial Proportion is(",round(ll,4),",",round(ul,4),")",sep=''))
                           }


binom_ci_ksd1<-function(x,alpha){
xbar=mean(x)
xsd=sd(x)
n<-length(x)
ll<-xbar-qt(alpha/2,n-1,lower.tail=FALSE)*xsd/sqrt(n)
ul<-xbar+qt(alpha/2,n-1,lower.tail=FALSE)*xsd/sqrt(n)
return(paste("The",100*(1-alpha),"%Confidence Interval for Binomial Proportion is(",round(ll,4),",",round(ul,4),")",sep=''))
                           }

n_lcd=893;x_lcd=39;p_lcd=0.04;
binom.test(n=n_lcd,x=x_lcd,p=p_lcd,alternative="greater")

n_doc<-119;x_doc<-38;p_doc<-0.2
binom.test(n=n_doc,x=x_doc,p=p_doc,alternative="two.sided")
t.

fff<-function(x){
for (i in 1:5)
ttt[i]<-chisq.test(UCBAdmissions[,,i])
return}o






