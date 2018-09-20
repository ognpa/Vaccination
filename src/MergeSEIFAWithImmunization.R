
#Read seifa for 2011 .The same file will be used till 2016 i.e <2016
seifa=read.csv("/Users/priya/Downloads/SEIFA_POA_15092018125111153.csv")
seifa=seifa[,c('Postal.Area.Code','Index.type','Measure','Time','Value')]
head(seifa)
dim(seifa)

#It is in long form lets widen it  we can see things like Score,Rank.within.Australia...Percentile etc

library(tidyr)
seifa_wide <- spread(seifa, Measure, Value)
#We get 4 rows for each index 
seifa_wide %>% group_by(Postal.Area.Code) %>% summarize(n())
head(seifa_wide)
dim(seifa_wide)

#Read immunisation data
aihw=read.csv("/Users/priya/Downloads/Vaccination/cleaned_data/immunisation_data.csv")
head(aihw)
aihw$postcode<-as.factor(aihw$postcode)


#We have two seifa files one for pre 2016 one for  2016 onwards which we will have to merge.
aihw_2016=aihw[aihw$year>2015,]
aihw_2015=aihw[aihw$year<=2015,]

seifa_wide$Postal.Area.Code<-as.factor(seifa_wide$Postal.Area.Code)
head(seifa_wide)

#Merge immunisation data for <2016
aihw_seifa_2015=merge(aihw_2015,seifa_wide,by.x='postcode',by.y='Postal.Area.Code',how='left')
dim(aihw_seifa_2015)  #Do due deligence checking TODO


#Seifa for 2016 onwards
seifa_new=read.csv("/Users/priya/Downloads/ABS_SEIFA2016_POA_15092018151227195.csv")
seifa_new=seifa_new[,c('Postal.Area.Code','Index.Type','Measure','Time','Value')]
head(seifa_new)
dim(seifa_new)

#Widen it so that we can see things like Score,Rank.within.Australia...Percentile etc
seifa_wide_new <- spread(seifa_new, Measure, Value)
head(seifa_wide_new)
dim(seifa_wide_new)

#Lets merge
aihw_seifa_2016=merge(aihw_2016,seifa_wide_new,by.x='postcode',by.y='Postal.Area.Code',how='left')
all_seifa<-rbind(aihw_seifa_2016,aihw_seifa_2015)

#Complained since column names were different
setdiff(aihw_seifa_2016, aihw_seifa_2015)
aihw_seifa_2015$postcode<-as.character(aihw_seifa_2015$postcode)
aihw_seifa_2016$postcode<-as.character(aihw_seifa_2016$postcode)

colnames(aihw_seifa_2016)
#Get column names we want had a V1 which was always NA so dropped it
aihw_seifa_2016=aihw_seifa_2016[,c('postcode','state','year','age','pc_immun','caution','pc_immun_class','PHN_code',
                   'PHN_number','Index.Type','Time','Maximum score for SA1s in area',
                   'Minimum score for SA1s in area','Rank within Australia',
                   'Rank within Australia - Decile','Rank within Australia - Percentile',
                   'Rank within State or Territory','Rank within State or Territory - Decile',
                   'Rank within State or Territory - Percentile','Score',
                   'Usual resident population')]

colnames(aihw_seifa_2016)=c('postcode','state','year','age','pc_immun','caution','pc_immun_class','PHN_code',
                            'PHN_number','Index.type','Time','Maximum score for SA1s in area',
                            'Minimum score for SA1s in area','Rank within Australia',
                            'Rank within Australia - Decile','Rank within Australia - Percentile',
                            'Rank within State or Territory','Rank within State or Territory - Decile',
                            'Rank within State or Territory - Percentile','Score',
                            'Usual resident population')
head(aihw_seifa_2016)

#Appending the dataframes together
all_seifa=rbind(aihw_seifa_2015,aihw_seifa_2016)
head(all_seifa)
all_seifa$postcode<-as.factor(all_seifa$postcode)

#Write this to a file
write.csv(all_seifa,"~/Downloads/Vaccination/cleaned_data/seifa_merged.csv")
se<-read.csv("~/Downloads/Vaccination/cleaned_data/seifa_merged.csv")
head(se)
