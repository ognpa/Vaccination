library(caret)
library(Amelia)
library(glmnet)
library(Boruta)


all_seifa<-read.csv("~/Downloads/Vaccination/cleaned_data/seifa_merged.csv")
all_seifa$postcode=as.factor(all_seifa$postcode)
all_seifa$target=0

all_seifa[all_seifa$pc_immun=='92.5-94.9','target']=1

head(all_seifa)
str(all_seifa)
ignore_cols=c('pc_immun','caution','pc_immun_class','Time',  'PHN_number','X')
colnames(all_seifa)
d<-all_seifa[ , -which(names(all_seifa) %in% ignore_cols)]
all_seifa=d
all_seifa=na.omit(all_seifa)

table(all_seifa$target)
trainIndex = createDataPartition(all_seifa$target,
                                 p=0.7, list=FALSE,times=1)
#all_seifa$postcode=as.numeric(all_seifa$postcode)
train = all_seifa[trainIndex,]
test = all_seifa[-trainIndex,]
train$src='train'
test$src='test'
all=rbind(train,test)
colnames(all)

train1=all[all$src=='train',-c(47,46)]
test1=all[all$src=='test',-c(47,46)]

train_y=all[all$src=='train','target']
test_y=all[all$src=='test','target']
library(Matrix)

train_sparse <- sparse.model.matrix(~ postcode+IRSD_SCORE ,train1)
test_sparse <- sparse.model.matrix(~  postcode+IRSD_SCORE ,test1)
fit <- cv.glmnet(train_sparse,train_y,nfolds=3)
coefList <- coef(fit, s='lambda.1se')
coefList <- data.frame(coefList@Dimnames[[1]][coefList@i+1],coefList@x)
names(coefList) <- c('var','val')

library(dplyr)
coefList %>%
  arrange(-abs(val)) %>%
  top_n(70)

pred <- predict(fit, test_sparse,type="response",s=fit$lambda.min)

prediction <- ifelse(pred >= 0.85, 1, 0) 
table(prediction)
confusionMatrix(as.factor(prediction),as.factor(test_y))

library(Metrics)
rmse(as.numeric(prediction), as.numeric(test_y))

library(ROCR)

ROCRpred <- prediction(pred, test_y)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
ROCRperf
auc_ROCR <- performance(ROCRpred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
