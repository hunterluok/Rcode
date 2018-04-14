
library(gbm)
trainset$churn<-ifelse(trainset$churn=="yes",1,0)
set.seed(2)

model6<-gbm(churn~.,distribution="bernoulli",data=trainset,n.trees=1000,interaction.depth=7,shrinkage=0.01,cv.folds=3)

model6c<-gbm.perf(model6,method="cv")

pred6<-predict(model6,testset,n.trees=model6c)
str(pred6)

roc6<-roc(testset$churn,pred6)

coords(roc6,"best")
churn.predict.class=ifelse(pred6>coords(roc6,"best")["threshold"],"yes","no")

table(testset$churn,churn.predict.class)