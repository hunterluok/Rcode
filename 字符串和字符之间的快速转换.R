d<-read.table(paste('http://archive.ics.uci.edu/ml/','machine-learning-databases/statlog/german/german.data',sep=''),stringsAsFactors=F,header=F)
print(d[1:3,])


mapping<-list("A11"="ÄÐ",'A12'='Å®','A13'='²©Ê¿','A14'='ÈËÑý')

if(class(d[,1])=="character") {d[,1]<-as.factor(as.character(mapping[d[,1]]))}