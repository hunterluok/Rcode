f<-function(x){
n<-nchar(x)
s<-c()                  #用来存在 排序后的  此处为字符串"a","b","c"
t1<-c()                 #初始为空，存储最终的 "abc"
for(i in 1:n){
s[n-i+1]<-substr(x,i,i)   # 调换语句 s
}
for(i in 1:n){
t1<-paste(t1,s[i],sep="") # 得到最终的 
}
t1
}
x<-c("123 45")
f(x)


h<-function(y){         #起到rev函数的作用
k<-length(y)         #向量中字符串的 长度
y1<-c()              #存放 排序后的几个字符串
y2<-c()              #存在最终的
for(i in 1:k){       #将几个字符串进行排序 y1
y1[i]<-y[k-i+1]
}
for(i in 1:k){
y2<-c(y2,f(y1[i])) #如果用y2<-paste(y2,f(y1[i]),sep="") 会将两个字符串合并成一个字符串)
}                  #调用上面的f函数 对单个字符串进行合并。
y2
}

y<-c("abc","def","ghi")
ly<-c("abc")
lly<-c(1,2,4);h(lly)  
h(y);h(ly)




