

x<-c('OXbf')
h<-function(x){
x1<-letters[1:6]
x3<-c(10:15)
n<-nchar(x)
s<-0
for(i in 3:n){
x2<-substring(x,i,i)
t1<-which(x1==x2)
t2<-x3[t1]
s<-s+t2*16^(n-i)
}
cat(s,"\n")
}
h(x)


x<-c('O15')
h<-function(x){
n<-nchar(x)
s<-0
for(i in 2:n){
x2<-substring(x,i,i)
t2<-as.numeric(x2)
s<-s+t2*8^(n-i)
}
cat(s,"\n")
}
h(x)
13