library(shiny)
library(raster)
library(leaflet)
library(rgeos)
library(readxl) 
library(maptools)
getwd()
load(file = "./data/current.rda")
load(file = "./data/2015.rda")

load(file = "./data/2014.rda")

load(file = "./data/2013.rda")

load(file = "./data/2012.rda")
library(hash)
h <- hash( keys=c("1","2","3","4","5"), values=c(tot_current,tot_2015,tot_2014,tot_2013,tot_2012))
#install.packages("rmapshaper")
library(rmapshaper)
bins <- c(86,88,90,92,94,96,98)

pal <- colorBin("YlOrRd", domain =tot_current$`Percent.fully.immunised....`, bins = bins)


ui <- fluidPage(
  
      selectInput("select", label = h3("Select box"), 
                  choices = list("2016-17" = 1, "2015-16" = 2, "2014-15" = 3,"2013-14"=4,"2012-13"=5), 
                  selected = 1),
      
      hr(),

      leafletOutput("mymap")
      
    
  
 

)

server <- function(input, output, session) {
 # output$value <- renderPrint({ input$select })  
  output$mymap <- renderLeaflet({
    reqd=input$select
    dt=h[[reqd]]
    
     plot1=leaflet(data = dt) %>% 
      addTiles() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      addPolygons(
        fillColor = ~pal(`Percent.fully.immunised....`),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7)%>%
      addLegend("bottomright", colors = "blue", labels = "PHN") 
    
    
    
  })
}


shinyApp(ui, server)
