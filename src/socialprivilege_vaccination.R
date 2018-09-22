setwd("/Users/priya/Downloads/Vaccination")
df<-read.csv("./cleaned_data/immunization_with_everything.csv")
head(df)
#Remove NP 
df=subset(df,df$pc_immun!='NP')
df$pc_immun<-factor(df$pc_immun)

plot(table(df$Rank.within.Australia,df$pc_immun))

colnames(df)
summary(df$Rank.within.Australia...Decile)
summary(df$Score)

d<-(prop.table(table(df$Rank.within.Australia...Decile,df$pc_immun),1)*100)
library(ggplot2)
head(d)

library(RColorBrewer)
# create a mosaic plot



spineplot(as.factor(df$Rank.within.Australia...Decile), as.factor(df$pc_immun))
library(ggplot2)
library(vcd)


mosaicplot(d,las = 2,     dir = c("h", "v"),
           col=brewer.pal(8,"Set1"),
           ,ylab="Rank within australia",
           xlab="Percentage immunized",
           off=30,main='Social privilege and vaccination',
           border = "chocolate") 
