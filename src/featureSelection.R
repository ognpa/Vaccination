#Univariate analysis
all_seifa<-read.csv("~/Downloads/Vaccination/cleaned_data/seifa_merged.csv")

head(all_seifa)
colnames(all_seifa)
str(all_seifa)
library(Hmisc)
describe(all_seifa)


head(all_seifa)
all_seifa$target=0

all_seifa[all_seifa$pc_immun=='92.5-94.9','target']=1
all_seifa=na.omit(all_seifa)


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(d, nc=10) 
table(all_seifa$target)
all_seifa=(all_seifa[,-c(1,3)])
head(all_seifa)
####Very unbalanced data set.... so we have 20% of rows where target=1
library(ROSE)
all_seifa$postcode=as.factor(all_seifa$postcode)
all_seifa$age=as.factor(all_seifa$age)
all_seifa$year=as.factor(all_seifa$year)

data.rose <- ROSE(target ~ ., data = all_seifa, seed = 1)$data
head(data.rose)
table(data.rose$target)
head(data.rose[data.rose$target==1,])

write.csv(data.rose,'~/Downloads/Vaccination/cleaned_data/balanced.csv')

