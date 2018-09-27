seifa=read.csv("/Users/priya/Downloads/SEIFA_POA_15092018125111153.csv")
seifa_new=read.csv("/Users/priya/Downloads/ABS_SEIFA2016_POA_26092018193603879.csv")

head(seifa)
df=seifa[,c(1,3,5,8,9)]
library(data.table)   # CRAN version 1.10.4
setDT(df)   # coerce to data.table
data_wide <- dcast(df, POA+Time~INDEX_TYPE+MEASURE, 
                   value.var = c("Value"))


seifa_wide=data_wide
head(seifa_new)
df_dash=seifa_new[,c(1,3,5,8,9)]
setDT(df_dash)   # coerce to data.table
data_wide_dash <- dcast(df_dash, POA+Time~SEIFAINDEXTYPE+SEIFA_MEASURE, 
                        value.var = c("Value"))
head(data_wide_dash)
#Read immunisation data
aihw=read.csv("/Users/priya/Downloads/Vaccination/cleaned_data/immunisation_data.csv")
head(aihw)
aihw$postcode<-as.factor(aihw$postcode)


#We have two seifa files one for pre 2016 one for  2016 onwards which we will have to merge.
aihw_2016=aihw[aihw$year>2015,]
aihw_2015=aihw[aihw$year<=2015,]


aihw_seifa_2015=merge(aihw_2015,data_wide,by.x='postcode',by.y='POA',how='left')
aihw_seifa_2016=merge(aihw_2016,data_wide_dash,by.x='postcode',by.y='POA',how='left')
setdiff(colnames(aihw_seifa_2016),colnames(aihw_seifa_2015))

all_seifa=rbind(aihw_seifa_2016,aihw_seifa_2015)
write.csv(all_seifa[,c(-10)],"~/Downloads/Vaccination/cleaned_data/seifa_economic_resources.csv")
head(all_seifa[,c(-10)])
all_seifa['target']=0
unique(all_seifa$pc_immun)
all_seifa[all_seifa$pc_immun=='95.0-100.0','target']=1
all_seifa[all_seifa$pc_immun=='92.5-94.9','target']=1
head(all_seifa)
ignore_cols=c('pc_immun','caution','pc_immun_class','Time','postcode','state','PHN_number'
)
colnames(all_seifa)
d<-all_seifa[ , -which(names(all_seifa) %in% ignore_cols)]


#Fit a logistic regression model
#install caret package
#load package
library(caret)
library(Amelia)



#missmap(d, main = "Missing values vs observed")
all_seifa=d
head(all_seifa$Time)
library(Boruta)
colnames(train)
d=na.omit(all_seifa)

boruta.train <- Boruta(as.factor(d$target) ~ ., data = d, doTrace = 2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)

unique(all_seifa$year)



#We are using t-1 to predict t ie 2015 to predict 2016
library(glmnet)


glm1 = glm(as.factor(target) ~ . ,family=binomial, data = all_seifa[all_seifa$year<=2016,])
summary(glm1)

pred=predict(glm1,newdata=all_seifa[all_seifa$year==2016,],type="response")
pred
prediction <- ifelse(pred >= 0.93, 1, 0) 
test=all_seifa[all_seifa$year==2016,]

confusionMatrix(as.factor(prediction),as.factor(all_seifa[all_seifa$year==2016,'target']))
library(ROCR)
ROCRpred <- prediction(pred, test$target)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
ROCRperf
