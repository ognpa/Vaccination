library(raster)
library(leaflet)
library(rgeos)
library(readxl) 
aus <- shapefile("/Users/priya/Downloads/PHN_boundaries_AUS_May2017_V7_Shapefile/PHN_boundaries_AUS_May2017_V7.shp")
aus_old <- shapefile("/Users/priya/Downloads/PHN_Shapefile_Sep2015_V5/PHN_boundaries_AUS_Sep2015_V5.shp")
df=read_excel("/Users/priya/Downloads/immunisationAIW.xlsx",
              skip=11,
              sheet='TAB 2')

aus$FIRST_PHN_=as.factor(aus$FIRST_PHN_)
df$`PHN code`=as.factor(df$`PHN code`)
aus_old$PHN_Code=as.factor(aus_old$PHN_Code)

a=df[df$`Reporting Year`=='2016-17' ,]

b=df[df$`Reporting Year`=='2012-13' 
     ,]


tot_current=merge(aus,
        a,
        by.x='FIRST_PHN_',
        by.y='PHN code',duplicateGeoms=TRUE)

tot_old=merge(aus_old,
              b,
              by.x='PHN_Code',
              by.y='PHN code',duplicateGeoms=TRUE)

bins <- c(86,88,90,92,94,96,98)
pal <- colorBin("YlOrRd", domain =a$`Percent fully immunised (%)`, bins = bins)

plot1=leaflet(data = tot_current) %>% 
    addTiles() %>% 
    addProviderTiles(providers$OpenStreetMap) %>% 
    addPolygons(fill = FALSE, stroke = TRUE, 
                color = "blue" ) %>%
    addLegend("bottomright", colors = "blue", labels = "PHN") 

plot1 %>% addPolygons(
    fillColor = ~pal(`Percent fully immunised (%)`),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)
  

pal <- colorBin("YlOrRd", domain =b$`Percent fully immunised (%)`, bins = bins)
library(maptools)
d_old=(coordinates(tot_old))
colnames(d_old)=c('longitude','latitude')
all_old=cbind(tot_old,d_old)
plot2=leaflet(data = all_old) %>% 
  addTiles() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(fill = FALSE, stroke = TRUE, 
              color = "blue" ) 
 # addLegend("bottomright", colors = "blue", labels = "PHN") %>%
addMarkers(all_old, lng = ~longitude, lat = ~latitude,label = ~all_old$PHN_Name)
plot2
plot2 %>% addPolygons(
  fillColor = ~pal(`Percent.fully.immunised....`),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)
require(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

df[,c('PHN code','Percent fully immunised (%)')]
heatmap(df[,c('PHN code','Percent fully immunised (%)','Reporting Year')])


library(httr)

	
login <- list(
  last_ym = "201808",
  REPORT_TYPE = "D",
  Sel_Month = "9",
  Sel_year="2018"
)

content(res)
res <- POST("http://www9.health.gov.au/cda/source/rpt_1.cfm", body = login, encode = "form", verbose())
team <- GET("http://kenpom.com/team.php?team=Rice", verbose())




library(rvest)
form = read_html("http://www9.health.gov.au/cda/source/rpt_1_sel.cfm") %>% html_node("form") %>% html_form()
f1 <- set_values(form,
                 last_ym = 201808,
                 REPORT_TYPE = D,
                 Sel_Month =9,
                 Sel_Year=2018)
f1$url <- url
cocorahs <- html_session('http://www9.health.gov.au/cda/source/rpt_1_sel.cfm')
submitted <- submit_form(cocorahs, form) 
t <- session %>% html_nodes("ta") %>%html_table() 


session 
