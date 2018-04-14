barplot(data2[,2],col="blue1",names.arg=data2[,2],width=1,space=0,ylim=c(0,10000),xlab="菜品",ylab="盈利：元")
accratio=data2[,2]

for(i in 1:length(accratio)){
accratio[i]=sum(data2[1:i,2])/sum(data2[,2])
}

par(new=T,mar=c(4,4,4,4))
points(accratio*10000~c((1:length(accratio)-0.5)),new=FALSE,type="b",new=T)
axis(4,col="red",col.axis="red",at=0:10000,label=c(0:10000/10000))
mtext("累计百分比",4,2)
points(6.5,accratio[7]*10000,col="red")
text(7,accratio[7]*10000,paste(round(accratio[7]+0.00001,4)*100,"%"))

