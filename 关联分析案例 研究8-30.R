


for(i in 1:4){
titanic.raw<-cbind(titanic.raw,rep(as.character(df[,i]),df$Freq))
}
titanic.raw  

titanic.raw<-as.data.frame(titanic.raw)
colnames(titanic.raw)<-names(df)[1:4]
dim(titanic.raw)
class(titanic.raw)
df1<-as.data.frame(titanic.raw)
library(arules)

rules<-apriori(df1,control=list(verbose=F),parameter=list(minlen=2,support=0.005,conf=0.8),
appearance=list(rhs=c("Survived=No","Survived=Yes"),default="lhs"))

rule.sort<-sort(rules,by="lift")

subset.matrix<-is.subset(rule.sort,rule.sort);subset.matrix

subset.matrix[lower.tri(subset.matrix,diag=T)]<-NA
redundant<-colSums(subset.matrix,na.rm=T)>=1
which(redundant)

rules.pruned<-rule.sort[!redundant]

rules<-apriori(df1,parameter=list(minlen=3,supp=0.002,conf=0.2),
appearance=list(rhs=c("Survived=Yes"),lhs=c("Class=1st","Class=2nd","Class=3rd","Age=Child","Age=Adult"),default="none"),
control=list(verbose=F))



rules1<-apriori(df1,parameter=list(minlen=3,supp=0.002,conf=0.2),
appearance=list(rhs=c("Survived=Yes"),lhs=c("Class=1st","Class=2nd","Class=3rd","Age=Child","Age=Adult")),
control=list(verbose=F))

install.packages("arulesViz")
library(arulesViz)
par(mfrow=c(1,2))
plot(rules)
plot(rules,method="grouped")
plot(rules,method="graph",control=list(type="items"))
plot(rules,method="graph")
plot(rules1,method="paracoord",control=list(reorder=TRUE))









