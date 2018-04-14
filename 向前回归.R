
backwardlm<-function(lm,criticalalpha){
   lm2=lm
   while(max(pvalueslm(lm2))>criticalalpha){
    lm2=update(lm2,paste(".~.-",attr(lm2$terms,"term.labels")[(which(pvalueslm(lm2)==max(pvalueslm(lm2))))],sep=""))}
return(lm2)
}

forwardlm<-function(y,x,criticalalpha){
yx<-data.frame(y,x)
mylm<-lm(y~-.,data=yx)
avail_cov<-attr(mylm$terms,"dataClasses")[-1]
minpvalues<-0
while(minpvalues<criticalalpha){
pvalues_curr<-NULL
for(i in 1:length(avail_cov)){
templm<-update(mylm,paste(".~.+",names(avail_cov[i])))
mypvalues<-summary(templm)$coefficients[,4]
pvalues_curr<-c(pvalues_curr,mypvalues[length(mypvalues)])
}
minpvalues<-min(pvalues_curr)
if(minpvalues<criticalalpha){
include_me_in<-min(which(pvalues_curr<criticalalpha))
mylm<-update(mylm,paste(".~.+",names(avail_cov[include_me_in])))
avail_cov<-avail_cov[-include_me_in]
}
}
return(mylm)
}
glf<-forwardlm(Gasoline$y,Gasoline[,-1],criticalalpha=0.2)

