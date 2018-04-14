tmp_data<-list(c("a"),c("a","b","c"),c("a","c"),c("d"),c("c","f"),c("a","d"),
c("c"),c("b","c"),c("a","e"),c("e","f"),c("a","b"),c("d","f"),c("c"),
c("b"),c("e"),c("g"),c("a","f"),c("c"),c("b"),c("c"))

names(tmp_data)=paste("tr",c(1:20),sep="")
trans<-as(tmp_data,"transactions")

transactionInfo(trans)$sequenceID=c(rep(1:4,each=5))
transactionInfo(trans)$eventID=c(10,20,30,40,50,10,20,30,40,10,20,30,40,50,
10,20,30,40,50,60)

transactionInfo(trans)$SIZE=c(1,3,2,1,2,2,1,2,2,2,2,2,1,1,1,1,2,1,1,1)


zaki<-read_baskets(con=system.file("misc","zaki.txt",package="arulesSequences"),info=c("sequenceID","eventID","SIZE"))
as(zaki,"data.frame")   #这里注意


sre<-cspade(trans,parameter=list(support=0.75),control=list(verbose=TRUE)) #这里注意

trans1<-trans
zaki1<-read_baskets(con=system.file("trans1","zaki1.txt",package="arulesSequences"),info=c("sequenceID","eventID","SIZE"))
as(zaki1,"data.frame")   #这里注意


