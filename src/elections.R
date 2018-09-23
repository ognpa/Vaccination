setwd("/Users/priya/Downloads/Vaccination")
aihw=read_excel("/Users/priya/Downloads/immunisationAIW.xlsx",
              skip=16,
              sheet='TAB 4')
aihw=subset(aihw,aihw$`Percent fully immunised (%)`!='NP')

electoral=read.csv("/Users/priya/Downloads/Vaccination/cleaned_data/elec_results_PC_all.csv")

head(electoral)

d<-merge(aihw,electoral,by.x='Postcode',by.y='Postcode')
e<-d[which(d$`Age group`=='1 year'),]
e1=unique(e[,c('Postcode','PartyAb2016','PartyAb2013','PartyAb2010','Percent fully immunised (%)')])

d2=na.omit(e1)
colnames(d2)=c('Postcode','2016','2013','2010','Percent fully immunised (%)')
library(reshape2)
d2_dash=(melt(d2, id.vars = c("Postcode", "Percent fully immunised (%)")))


political_party_df=ggplot(data=d2_dash, aes(d2_dash$`Percent fully immunised (%)`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+
  facet_grid(d2_dash$variable~d2_dash$value)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df


ggsave("electoral.png",political_party_df)
getwd()
