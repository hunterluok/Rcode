d<-read.table(paste('http://archive.ics.uci.edu/ml/','machine-learning-databases/statlog/german/german.data',sep=''),stringsAsFactors=F,header=F)
print(d[1:3,])


mapping<-list("A11"="��",'A12'='Ů','A13'='��ʿ','A14'='����')

if(class(d[,1])=="character") {d[,1]<-as.factor(as.character(mapping[d[,1]]))}