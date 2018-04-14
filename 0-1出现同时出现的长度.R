maxheads<-function(n.toss){
n_heads=0;
max_heads=0;
for(i in 1:n.toss){
if(runif(1)<0.5){
n_heads<-n_heads+1
}else{
n_heads<-0
}
if(n_heads>max_heads){
max_heads<-n_heads
}
}
return(max_heads)
}
maxheads(20)