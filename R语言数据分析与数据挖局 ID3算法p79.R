calculateentropy<-function(data){
t<-table(data)
sum<-sum(t)
t<-t[t!=0]
entropy<--sum(log2(t/sum)*(t/sum))
return(entropy)
}

calculateentropy2<-function(data){
var<-table(data[1])
p<-var/sum(var)
varnames<-names(var)
array<-c()
for(name in varnames){
array<-append(array,calculateentropy(subset(data,data[1]==name,select=2)))
}
return(sum(array*p))
}

buildtree<-function(data){
if(length(unique(data$result))==1){
cat(data$result[1])
return()
}
if(length(names(data))==1){
cat("...")
return()
}
entropy<-calculateentropy(data$result)
labels<-names(data)
label<-""
temp<Inf
subentropy<-c()
for(i in 1:(length(data)-1)){
temp2<-calculateentropy2(data[c(i,length(labels))])
if(temp2<temp){
temp<-temp2
label<-labels[i]
}
subentropy<-append(subentropy,temp)}
cat(label)
cat("[")
nextlabels<-labels[labels!=label]
for(value in unlist(unique(data[label]))){
cat(value,":")
buildtree(subset(data,data[label]==valuem,select=nextlabels))
cat(";")
}
cat("]")
}
buildtree(data1)

