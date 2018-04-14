


auto<-within(auto,cylinders<-relevel(cylinders,ref="4cyl"))
mode<-lm(mpg~.,data=auto[t.idx,-c(1,8,9)])
summary(mode)