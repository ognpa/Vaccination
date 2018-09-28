seifa=read.csv("~/Downloads/Vaccination/raw_data/SEIFA_POA_15092018125111153.csv")
seifa_new=read.csv("~/Downloads/Vaccination/raw_data/ABS_SEIFA2016_POA_26092018193603879.csv")

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

#Write this to a file
write.csv(all_seifa,"~/Downloads/Vaccination/cleaned_data/seifa_merged.csv")
se<-read.csv("~/Downloads/Vaccination/cleaned_data/seifa_merged.csv")
head(se)
colnames(se)
