±´Ò¶Ë¹ÅÐ±ð£º

library(e1071)
classifier<-naiveBayes(train[,!names(train) %in% c("churn")],train$churn)
classifier

bayestable<-table(predict(classifier,test[,!names(test) %in% c("churn")]),test$churn)
bayestable