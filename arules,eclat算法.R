
data(Groceries)
summary(Groceries)

itemFrequencyPlot(Groceries,support=0.1,cex.names=0.8,topN=5,col=14)

model<-apriori(Groceries,parameter=list(supp=0.001,conf=0.5,target="rules"))##target=frequent itemsets

##[[[tt<-sort(model,by=c("confidence","lift"))
inspect(head(tt))
tt<-sort(model,by=c("lift"))
inspect(head(tt))]]]


rule1<-apriori(Groceries,parameter=list(supp=0.001,conf=0.1,maxlen=2),
appearance=list(rhs="mustard",default="lhs"))

可视化
library(arulesViz)
plot(rule1,measure=c("support","lift"),shading="confidence")

plot(model,interactive=T)
plot(model,method="grouped")

rule5<-apriori(Groceries,parameter=list(support=0.002,conf=0.5))
plot(rule5[1:50],method="matrix",measure="lift")
plot(rule5[1:50],method="paracoord")

rules.sorted<-sort(model,by="lift")
subset.matrix<-is.subset(rules.sorted,rules.sorted)
subset.matrix[lower.tri(subset.matrix,diag=T)]=NA
redundant<-colSums(subset.matrix,na.rm=T)>=1

rules.pruned<-rules.sorted[!redundant]

inspect(head(rules.pruned))

soda<-apriori(Groceries,parameter=list(supp=0.001,conf=0.1,minlen=2),appearance=list(default="rhs",lhs="soda"))


plot(sort(soda,by="lift"),method="graph",control=list(type="items"))#有意思
plot(soda,method="grouped") ##有问题


fre<-eclat(Groceries,parameter=list(supp=0.05,maxlen=10))

summary(fre)

inspect(sort(fre,by="support")[1:10])














