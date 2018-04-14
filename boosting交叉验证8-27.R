set.seed(2)
model4<-boosting(churn~.,trainset,mfinal=10,coeflearn="Freund",boos=FALSE,control=rpart.control(maxdepth=3))

pred3<-predict.boosting(model4,testset)
pred3
#等价的 pred3d<-predict(model4,testset)

pred3$confusion 
pred3$error


library(pROC)
library(mboost)  #这里需要注意
set.seed(2)
ctrl<-trainControl(method="repeatedcv",repeats=1,classProbs=TRUE,summaryFunction=twoClassSummary)
ada.train=train(churn~.,trainset,method="ada",metric="ROC",trControl=ctrl)
ada.train$result  

plot(ada.train)
pred5<-predict(ada.train,testset,"prob")
predresult<-ifelse(pred5[1]>0.5,"yes","no")
table(predresult,testset$churn)
pred51<-predict(ada.train,testset)

model5<-boosting.cv(churn~.,v=10,trainset,mfinal=5,control=rpart.control(cp=0.01))
model5$confusion
model5$error
