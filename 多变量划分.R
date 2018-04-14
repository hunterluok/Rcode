loglikelyhood<-function(outcol,predcol){
sum(ifelse(outcol==pos,log(predcol),log(1-predcol)))
}


selvars<-c()
minstep<-5
baseratecheck<-loglikelyhood(dcal[,outcome],sum(dcal[,outcome]==pos)/length(dcal[,outcome]))

for(v in catvars){
pi<-paste('pred',v,sep='')
licheck<-2*((loglikelyhood(dcal[,outcome],dcal[,pi])-baseratecheck))
if(licheck>minstep){
print(sprintf("%s,calibrationscore:%g",pi,licheck))
selvars<-c(selvars,pi)
}
}

for(v in numericvars){
pi<-paste('pred',v,sep='')
licheck<-2*((loglikelyhood(dcal[,outcome],dcal[,pi])-baseratecheck)-1)
if(licheck>=minstep){
print(sprintf("%s,calibrationscore:%g",pi,licheck))
selvars<-c(selvars,pi)
}
}