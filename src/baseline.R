all_seifa<-read.csv("~/Downloads/Vaccination/cleaned_data/seifa_merged.csv")
library(caret)


all_seifa$target=0

all_seifa[all_seifa$pc_immun=='92.5-94.9','target']=1
all_seifa$target=as.factor(all_seifa$target)

table(all_seifa$target)

#So if we blindly guess we should be right about 80 percent of the time
all_seifa['prediction']=0

preds <- factor(all_seifa$prediction, levels = c("0","1"))

all_seifa$prediction=preds

confusionMatrix(as.factor(all_seifa$prediction),as.factor(all_seifa$target))
library(Metrics)
rmse(as.numeric(all_seifa$prediction), as.numeric(all_seifa$target))

#0.4098
