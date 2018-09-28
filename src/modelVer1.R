all_seifa<-read.csv("balanced.csv")
all_seifa$postcode=as.factor(all_seifa$postcode)

head(all_seifa)
str(all_seifa)
ignore_cols=c('pc_immun','caution','pc_immun_class','Time',
              'PHN_number','state','PHN_code','PHN_number')
colnames(all_seifa)
d<-all_seifa[ , -which(names(all_seifa) %in% ignore_cols)]
head(d)
#Fit a logistic regression model
#install caret package
#load package
library(caret)
library(Amelia)
all_seifa=d

library(glmnet)
library(Boruta)
d=na.omit(all_seifa)
d$target<-as.factor(d$target)
str(d)


all_seifa=na.omit(all_seifa)

trainIndex = createDataPartition(all_seifa$target,
                                 p=0.7, list=FALSE,times=1)
#all_seifa$postcode=as.numeric(all_seifa$postcode)
train = all_seifa[trainIndex,]
test = all_seifa[-trainIndex,]
train$src='train'
test$src='test'
all=rbind(train,test)
colnames(all)
train=all[all$src=='train',-c(44,45)]
test=all[all$src=='test',-c(44,45)]

train_y=all[all$src=='train','target']
test_y=all[all$src=='test','target']
library(Matrix)

train_sparse <- sparse.model.matrix(~.,train)
test_sparse <- sparse.model.matrix(~.,test)
fit <- cv.glmnet(train_sparse,train_y,nfolds=3)
pred <- predict(fit, test_sparse,type="response",s=fit$lambda.min)
prediction <- ifelse(pred >= 0.9, 1, 0) 
table(prediction)
confusionMatrix(as.factor(prediction),as.factor(test_y))
library(ROCR)

ROCRpred <- prediction(pred, test_y)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
ROCRperf

