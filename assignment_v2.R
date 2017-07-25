cat("\014")
rm(list=ls())

set.seed(345)
training = read.csv("C:\\datascience\\ML\\week4\\assignment\\pml-training.csv",na.strings=c("NA","#DIV/0!", ""))

testing = read.csv("C:\\datascience\\ML\\week4\\assignment\\pml-testing.csv",na.strings=c("NA","#DIV/0!", ""))


library(caret)
library(randomForest)
library(e1071)
library(gbm)

dim(training)

plot(colSums(is.na(training)))

training<-training[,colSums(is.na(training))<19000]

dim(training)


plot(colSums(is.na(testing)))

testing<-testing[,colSums(is.na(testing))!=20]
dim(testing)


training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]

#cross-validation sampling

cvsamples <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
cvtraining <- training[cvsamples, ] 
cvtesting <- training[-cvsamples, ]


mod_rf<- randomForest(classe ~. , data=cvtraining, method="class")
mod_svm <- svm(classe ~. , data=cvtraining)

pred_rf <- predict(mod_rf, cvtesting, type = "class")
confusionMatrix(pred_rf, cvtesting$classe)



pred_svm <- predict(mod_svm, cvtesting, type = "class")
confusionMatrix(pred_svm, cvtesting$classe)



 predict(mod_rf, testing)
