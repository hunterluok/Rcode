sim<-function(nreps){
commdata<-list()
commdata$countabsamecomm<-0
for(rep in 1:nreps){
commdata$whosleft<-1:20
commdata$numabchosen<-0
commdata<-choosecomm(commdata,5)
if(commdata$numabchosen>0) next
commdata<-choosecomm(commdata,4)
if(commdata$numabchoosen>0) next
commdata<-choosecomm(commdata,3)
}
print(commdata$countabsamecomm/nreps)
}

choosecomm<-function(comdat,comsize){
committee<-sample(comdat$whoslest,comsize)
comdat$numabchosen<-length(intersect(1:2,committee))
if(comdat$numabchosen==2)
comdat$countabsamecomm<-comdat$countabsamecomm+
comdat$whosleft<-setdiff(comdat$whosleft,committee)
return(comdat)
}