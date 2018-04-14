
mk<-function(Userid,Itemid,n,k){
sub<-which(data$userid==Userid)
if(length(sub)>=n) sub_n=sample(sub,n)
if(length(sub)<n) sun_n=sample(sub,length(sub))
known_itemid=data$itemid[sub_n]  #sub_n是 目标看过的电影数量。
unknow_itemid=Itemid        #目标 没看的  找到其他人看了的
know_itemid
unknown_itemid
unknow_sub<-which(data$itemid==unknow_itemid)
user=data$userid[unknown_sub[-1]]
user                       #找到看过该电影的 其他人
data_all=matrix(0,1+length(user),2+length(known_itemid)) # 构造矩阵 行为看过这部电影的人+目标
data_all=data.frame(data_all)
names(data_all)=c("userid",paste("unknow_itemid_",Itemid),paste("itemid_",known_itemid,sep=""))
item=c(unknown_itemid,known_itemid)
data_all$userid=c(Userid,user)
data_all
for(i in 1:nrow(data_all)){
data_temp=data[which(data$userid==data_all$userid[i]),]
for(j in 1:length(item))
{ if(sum(as.numeric(data_temp$itemid==item[j]))!=0)
(data_all[i,j+1]=data_temp$ratind[which(data_temp$itemid==item[j])]}
}}
data_all
data_test_x<-data_all[1,c(-1,-2)]
data_test_y<-data_all[1,2]
data_train_x<-data_all[-1,c(-1,-2)]
data_train_y<-data_all[-1,2]
dim(data_test_x);length(data_test_y)
fit<-knn(data_train_x,data_test_x,ck=data_train_y,k=K)
list("dat_aLL:"=data_all,"True Ratinf:"=data_test_y,"Predict Rating:"=fit,
"User ID:"=Userid,"Item id:"=Itemid)
}