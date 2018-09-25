
library(raster)
library(leaflet)
library(readxl)
library(rgeos)
library(rmapshaper)
aus <- shapefile("/Users/priya/Downloads/SA3/SA3_2016_AUST.shp")
aus_old<-shapefile("/Users/priya/Downloads/sa3_old/SA3_2011_AUST.shp")
df=read_excel("/Users/priya/Downloads/immunisationAIW.xlsx",
              skip=13,
              sheet='TAB 3')

unique(df$`Reporting Year`)
aus$SA3_CODE16=as.factor(aus$SA3_CODE16)
df$`SA3 code`=as.factor(df$`SA3 code`)
a=df[(df$`Reporting Year`=='2016-17') & (df$`Age group`=='1 year') ,]
b=df[(df$`Reporting Year`=='2015-16') & (df$`Age group`=='1 year') ,]
c=df[(df$`Reporting Year`=='2014-15') & (df$`Age group`=='1 year') ,]
d=df[(df$`Reporting Year`=='2013-14')& (df$`Age group`=='1 year') ,]
e=df[(df$`Reporting Year`=='2012-13')& (df$`Age group`=='1 year') ,]



all_old=rbind(b,c,d,e)
df$`SA3 code`<-as.factor(df$`SA3 code`)
tot_current=merge(aus,
                  a,
                  by.x='SA3_CODE16',
                  by.y='SA3 code',duplicateGeoms=TRUE,how='left')
tot_current$SA3_CODE16<-as.factor(tot_current$SA3_CODE16)

tot_current <- ms_simplify(tot_current, keep = 0.01)
save(tot_current,file='current.rda')

tot_2015=merge(aus_old,
               b,
               by.x='SA3_CODE11',
               by.y='SA3 code',duplicateGeoms=TRUE)
tot_2014=merge(aus_old,
               c,
               by.x='SA3_CODE11',
               by.y='SA3 code',duplicateGeoms=TRUE)
tot_2013=merge(aus_old,
               d,
               by.x='SA3_CODE11',
               by.y='SA3 code',duplicateGeoms=TRUE)


tot_2012=merge(aus_old,
               e,
               by.x='SA3_CODE11',
               by.y='SA3 code',duplicateGeoms=TRUE)


load(file = "./current.rda")
tot_current=tot_current[which(tot_current$`Percent fully immunised (%)`!='NP'),]
tot_current$`Percent fully immunised (%)`=as.numeric(tot_current$`Percent fully immunised (%)`)

tot_2012=tot_2012[which(tot_2012$`Percent fully immunised (%)`!='NP'),]
tot_2012$`Percent fully immunised (%)`<-as.numeric(tot_2012$`Percent fully immunised (%)`)

library(leaflet)
bins <- c(78,80,82,84,86,88,90,92,94,96,98)

pal <- colorBin("YlOrRd", domain =tot_current$`Percent fully immunised (%)`, bins = bins)
#grab a palette
library(RColorBrewer)

pal <- brewer.pal(11, "YlOrRd")

#now make it more continuous 
#as a colorRamp
pal <- colorRampPalette(pal)
plot1=leaflet(data = tot_current) %>% 
  addTiles() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(
    fillColor = ~pal(`Percent fully immunised (%)`),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)%>%
  addLegend("bottomright", colors = "blue", labels = "PHN") 
plot1
