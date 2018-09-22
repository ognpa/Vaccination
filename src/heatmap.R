
####Public policy and vaccination
library(httr)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(rvest)
library("ggplot2");
library(zoo)
disease_names=c("diptheria",
                "hib","measles","mumps",'pneumococal',
                "pertussis",
                "Rota virus","rubella","tetanus",
                "chicken pox",
              # "polio",
               "hepatitis")
disease_num=c('009',
  "012", "021","043","065",
  "024",
              "077","029",
              "033","073",
            #  "026",
  "039")
names(disease_num)=disease_names
library(reshape2)
df<-melt(disease_num)
df$disease<-rownames(df)
url <- "http://www9.health.gov.au/cda/source/rpt_3_sel.cfm"
cocorahs <- html_session(url)
form.unfilled <- cocorahs %>% html_node("form") %>% html_form()

getstuff <- function(code){
 

  print(code)
# Specify URL
 
# Grab Initial Form
#  Form is filled in stages. Here, only do country and date
  form.filled <- form.unfilled %>%
    set_values(
                   CAUSE=code,REPORT_OPTION=1)

# submit the form and save as a new session
  session <- submit_form(cocorahs, form.filled) 

# look for a table in the nodes
  table <- session %>% html_nodes("table")

  print("in here")
# The table you want
  df<-table[[1]] %>% html_table()
  colnames(df)
  colnames(df)<-c("Year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","AnnualTotal")

  dd=(spread(df[,c("Year",'AnnualTotal')],key = as.numeric(Year), value = AnnualTotal))
  print(head(dd))
  return(dd)

}



all_df=lapply(df$value,getstuff)%>% bind_rows()

mat<-as.matrix(all_df)
matnorm<-mat/rowSums(mat,na.rm=T)
datnorm<-as.data.frame(matnorm)

head(all_df)


all_reqd=cbind(df,datnorm)
mat=data.matrix(all_reqd[,-c(1,2)])
colnames(all_reqd)
f<-melt(all_reqd[,-c(1)])
head(f)

cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e"))(10),
          colorRampPalette(c("#eec73a", "#e29421", "#e29421", "#f05336","#ce472e"), bias=2)(90))
                             
a<-ggplot(f, aes((variable), disease, fill=value)) +
  geom_tile(colour="white", size=0.25) + 
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  labs(x="Disease",y="Year",title="Notifications of vaccinated diseases in Australia")+
  #remove extra space
  #custom breaks on x-axis
  scale_x_discrete(expand=c(0,0))+
  #maintains aspect ratio.
  coord_fixed()+
  #set a base size for all fonts
  theme_grey(base_size=8)+
  #theme options
  theme(
    #bold font for both axis text
    axis.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())+
  scale_fill_gradientn(colours=cols, limits=c(0, 0.3),
                       breaks=seq(0.001, 0.276, by=0.01), 
                       #values=c(0,0.01, 0.02, 0.03,0.04,0.05,0.06,0.07,0.08, 0.09,0.1,0.12,0.15,0.175,0.2,0.25,0.3), 
                       na.value=rgb(192,192,192 ,max=255))+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("vaccination_notification.png",a)

  