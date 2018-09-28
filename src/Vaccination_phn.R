aus <- shapefile("/Users/priya/Downloads/PHN_boundaries_AUS_May2017_V7_Shapefile/PHN_boundaries_AUS_May2017_V7.shp")
aus_old <- shapefile("/Users/priya/Downloads/PHN_Shapefile_Sep2015_V5/PHN_boundaries_AUS_Sep2015_V5.shp")
df=read_excel("/Users/priya/Downloads/immunisationAIW.xlsx",
              skip=11,
              sheet='TAB 2')

unique(df$`Reporting Year`)
aus$FIRST_PHN_=as.factor(aus$FIRST_PHN_)
df$`PHN code`=as.factor(df$`PHN code`)
aus_old$PHN_Code=as.factor(aus_old$PHN_Code)


unique(df$`Reporting Year`)

a=df[df$`Reporting Year`=='2016-17' ,]
b=df[df$`Reporting Year`=='2015-16' ,]
c=df[df$`Reporting Year`=='2014-15' ,]
d=df[df$`Reporting Year`=='2013-14' ,]
e=df[df$`Reporting Year`=='2012-13' ,]



all_old=rbind(b,c,d,e)

tot_current=merge(aus,
                  a,
                  by.x='FIRST_PHN_',
                  by.y='PHN code',duplicateGeoms=TRUE)
tot_2015=merge(aus_old,
              b,
              by.x='PHN_Code',
              by.y='PHN code',duplicateGeoms=TRUE)
tot_2014=merge(aus_old,
             c,
             by.x='PHN_Code',
             by.y='PHN code',duplicateGeoms=TRUE)
tot_2013=merge(aus_old,
             d,
             by.x='PHN_Code',
             by.y='PHN code',duplicateGeoms=TRUE)


tot_2012=merge(aus_old,
             e,
             by.x='PHN_Code',
             by.y='PHN code',duplicateGeoms=TRUE)

tot_current <- ms_simplify(tot_current, keep = 0.01)
save(tot_current,file='current.rda')

tot_2015 <- ms_simplify(tot_2015, keep = 0.01)
save(tot_2015,file='2015.rda')

tot_2014 <- ms_simplify(tot_2014, keep = 0.01)
save(tot_2014,file='2014.rda')

tot_2013 <- ms_simplify(tot_2013, keep = 0.01)
save(tot_2013,file='2013.rda')

tot_2012 <- ms_simplify(tot_2012, keep = 0.01)

save(tot_2012,file='2012.rda')

getwd()
rm(list=ls())
load('2015.rda')
