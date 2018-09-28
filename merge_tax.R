library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
options(scipen=999)

immu_clean_raw<-read.csv("immunization_with_everything.csv")
summary(immu_clean_raw)
str(immu_clean_raw) 

summary(immu_clean_raw$postcode)
summary(immu_clean_raw$postcode)


######Import Taxation in 2016#####
tax_2016<-read_xlsx("2016.xlsx", sheet="Individuals Table 6B")
#Select only Postcode, Taxable Income and Number of individuals
#rename them first for easy to merge
tax_2016<-tax_2016%>%
  rename("state"= "State/ Territory1")%>%
  rename("num_individuals"= "Number of individuals no.")%>%
rename("total_tax"= "Taxable income or loss $")%>%
rename("postcode"="Postcode")


#remove any " , " in these two columns and convert to number
tax_2016$total_tax<-as.numeric(tax_2016$total_tax)
tax_2016$total_tax<-gsub(",","",tax_2016$total_tax)
tax_2016$num_individuals<-as.numeric(tax_2016$num_individuals)
tax_2016$total_tax<-as.numeric(tax_2016$total_tax)

#filter out uneccessary column
tax_2016<-tax_2016%>%
  select(state,postcode,num_individuals,total_tax)

# create new column for mean values and year
tax_2016_final<-tax_2016%>%
  group_by(postcode)%>%
mutate(mean_tax= ceiling(total_tax/num_individuals))%>%
  mutate(year="2016")
#convert integert to have the same column character
tax_2016_final$year<-as.integer(tax_2016_final$year)
tax_2016_final$postcode<-as.integer(tax_2016_final$postcode)
#This introduce some NAs, which include non postcode one, let's remove them
any(is.na(tax_2016_final$postcode))
sum(is.na(tax_2016_final$postcode))
tax_2015_final<-na.omit(tax_2016_final)

# Create Immu data for only 2016
immu_2016<-immu_clean_raw%>%
     filter(year==2016)

#Join data with immuni_clean
combine.2016<-left_join(immu_2016,tax_2016_final,by=c("year","postcode"))
######END 2016######


#####Import Taxation in 2015######
tax_2015<-read_xlsx("2015.xlsx", sheet="Individuals Table 6B")
names(tax_2015)
summary(tax_2015$postcode)
#rename them first for easy to merge
tax_2015<-tax_2015%>%
  rename("state"= "State/ Territory1")%>%
  rename("num_individuals"= "Number of individuals\r\nno.")%>%
  rename("total_tax"= "Taxable income or loss3\r\n$")%>%
  rename("postcode"="Postcode")

#remove any " , " in these two columns and convert to number
tax_2015$total_tax<-as.numeric(tax_2015$total_tax)
tax_2015$total_tax<-gsub(",","",tax_2015$total_tax)
tax_2015$num_individuals<-as.numeric(tax_2015$num_individuals)
tax_2015$total_tax<-as.numeric(tax_2015$total_tax)



#filter out uneccessary column
tax_2015<-tax_2015%>%
  select(state,postcode,num_individuals,total_tax)

# create new column for mean values and year
tax_2015_final<-tax_2015%>%
  group_by(postcode)%>%
  mutate(mean_tax= ceiling(total_tax/num_individuals))%>%
  mutate(year="2015")

#convert integert to have the same column character
tax_2015_final$year<-as.integer(tax_2015_final$year)
tax_2015_final$postcode<-as.integer(tax_2015_final$postcode)
#This introduce some NAs, which include non postcode one, let's remove them
any(is.na(tax_2015_final$postcode))
sum(is.na(tax_2015_final$postcode))
tax_2015_final<-na.omit(tax_2015_final)

#create immuni data for 2015 only
immu_2015<-immu_clean_raw%>%
  filter(year==2015)

#Join data immuni_2015 with tax_2015_final
combine.2015<-left_join(immu_2015,tax_2015_final,by=c("year","postcode"))
#####END 2015 ####


#### Import taxtation 2014 #####
tax_2014<-read_xlsx("2014.xlsx", sheet="Table 6")
names(tax_2014)
str(tax_2014$postcode)
summary(tax_2014$postcode)
glimpse(tax_2014$`Taxable Status`)
#rename them first for easy to merge
tax_2014<-tax_2014%>%
  rename("state"= "State/Territory1")%>%
  rename("num_individuals"= "Number of individuals")%>%
  rename("total_tax"= "Taxable income or loss3\r\n$")%>%
  rename("postcode"="Postcode")%>%
rename("taxable_stat"="Taxable Status")
#remove any " , " in these two columns and convert to number
tax_2014$total_tax<-as.numeric(tax_2014$total_tax)
tax_2014$total_tax<-gsub(",","",tax_2014$total_tax)
tax_2014$num_individuals<-as.numeric(tax_2014$num_individuals)
tax_2014$total_tax<-as.numeric(tax_2014$total_tax)



#filter out uneccessary column
tax_2014<-tax_2014%>%
  select(state,postcode,num_individuals,total_tax)
#extra step because 2014 doesn't combine the data Taxable and Non Tax
tax_2014_test<-tax_2014 %>% 
  group_by(postcode,state) %>% 
  summarise(total_tax = sum(total_tax),num_individuals = sum(num_individuals))

# create new column for mean values and year
tax_2014_final<-tax_2014_test%>%
  group_by(postcode)%>%
  mutate(mean_tax= ceiling(total_tax/num_individuals))%>%
  mutate(year="2014")




#convert integert to have the same column character
tax_2014_final$year<-as.integer(tax_2014_final$year)
tax_2014_final$postcode<-as.integer(tax_2014_final$postcode)
#This introduce some NAs, which include non postcode one, let's remove them
any(is.na(tax_2014_final$postcode))
sum(is.na(tax_2014_final$postcode))
tax_2014_final<-na.omit(tax_2014_final)

#create immuni data for 2014 only
immu_2014<-immu_clean_raw%>%
  filter(year==2014)

#Join data immuni_2014 with tax_2014_final
combine.2014<-left_join(immu_2014,tax_2014_final,by=c("year","postcode"))
#####END 2014 ####
#### Import taxtation 2013 #####
tax_2013<-read_xlsx("2013.xlsx", sheet="Postcode only")
names(tax_2013)
str(tax_2013$postcode)
summary(tax_2013$postcode)
glimpse(tax_2013$`Taxable Status`)
#rename them first for easy to merge
tax_2013<-tax_2013%>%
  rename("state"= "State/Territory1")%>%
  rename("num_individuals"= "Number of individuals\r\nno.")%>%
  rename("total_tax"= "Taxable income or loss\r\n$")%>%
  rename("postcode"="Postcode")

#remove any " , " in these two columns and convert to number
tax_2013$total_tax<-as.numeric(tax_2013$total_tax)
tax_2013$total_tax<-gsub(",","",tax_2013$total_tax)
tax_2013$num_individuals<-as.numeric(tax_2013$num_individuals)
tax_2013$total_tax<-as.numeric(tax_2013$total_tax)



#filter out uneccessary column
tax_2013<-tax_2013%>%
  select(state,postcode,num_individuals,total_tax)

# create new column for mean values and year
tax_2013_final<-tax_2013%>%
  group_by(postcode)%>%
  mutate(mean_tax= ceiling(total_tax/num_individuals))%>%
  mutate(year="2013")

#convert integert to have the same column character
tax_2013_final$year<-as.integer(tax_2013_final$year)
tax_2013_final$postcode<-as.integer(tax_2013_final$postcode)
#This introduce some NAs, which include non postcode one, let's remove them
any(is.na(tax_2013_final$postcode))
sum(is.na(tax_2013_final$postcode))
tax_2013_final<-na.omit(tax_2013_final)

#create immuni data for 2013 only
immu_2013<-immu_clean_raw%>%
  filter(year==2013)

#Join data immuni_2013 with tax_2013_final
combine.2013<-left_join(immu_2013,tax_2013_final,by=c("year","postcode"))
#####END 2013 ####
#### Import taxtation 2012 #####
tax_2012<-read_xlsx("2012.xlsx", sheet="Individuals Tax Table 6")
names(tax_2012)
str(tax_2012$postcode)
summary(tax_2012$postcode)
glimpse(tax_2012$`Taxable Status`)
#rename them first for easy to merge
tax_2012<-tax_2012%>%
  rename("state"= "State/Territory1")%>%
  rename("num_individuals"= "Number of individuals\r\nno.")%>%
  rename("total_tax"= "Taxable income or loss3\r\n$")%>%
  rename("postcode"="Postcode")

#remove any " , " in these two columns and convert to number
tax_2012$total_tax<-as.numeric(tax_2012$total_tax)
tax_2012$total_tax<-gsub(",","",tax_2012$total_tax)
tax_2012$num_individuals<-as.numeric(tax_2012$num_individuals)
tax_2012$total_tax<-as.numeric(tax_2012$total_tax)



#filter out uneccessary column
tax_2012<-tax_2012%>%
  select(state,postcode,num_individuals,total_tax)


#extra step because 2014 doesn't combine the data
tax_2012_test<-tax_2012 %>% 
  group_by(postcode,state) %>% 
  summarise(total_tax = sum(total_tax),num_individuals = sum(num_individuals))

# create new column for mean values and year
tax_2012_final<-tax_2012_test%>%
  group_by(postcode)%>%
  mutate(mean_tax= ceiling(total_tax/num_individuals))%>%
  mutate(year="2012")

#convert integert to have the same column character
tax_2012_final$year<-as.integer(tax_2012_final$year)
tax_2012_final$postcode<-as.integer(tax_2012_final$postcode)
#This introduce some NAs, which include non postcode one, let's remove them
any(is.na(tax_2012_final$postcode))
sum(is.na(tax_2012_final$postcode))
tax_2012_final<-na.omit(tax_2012_final)

#create immuni data for 2012 only
immu_2012<-immu_clean_raw%>%
  filter(year==2012)

#Join data immuni_2012 with tax_2012_final
combine.2012<-left_join(immu_2012,tax_2012_final,by=c("year","postcode"))

#####END 2012 ####

#### Import taxtation 2011 #####
tax_2011<-read_xlsx("2011.XLSX", sheet="Individuals Tax Table 6a")
names(tax_2011)
str(tax_2011$postcode)
summary(tax_2011$postcode)
glimpse(tax_2011$`Taxable Status`)
#rename them first for easy to merge
tax_2011<-tax_2011%>%
  rename("state"= "State/Territory")%>%
  rename("num_individuals"= "Number of individuals")%>%
  rename("total_tax"= "Taxable income or loss")%>%
  rename("postcode"="Postcode")

#remove any " , " in these two columns and convert to number
tax_2011$total_tax<-as.numeric(tax_2011$total_tax)
tax_2011$total_tax<-gsub(",","",tax_2011$total_tax)
tax_2011$num_individuals<-as.numeric(tax_2011$num_individuals)
tax_2011$total_tax<-as.numeric(tax_2011$total_tax)



#filter out uneccessary column
tax_2011<-tax_2011%>%
  select(state,postcode,num_individuals,total_tax)

# create new column for mean values and year
tax_2011_final<-tax_2011%>%
  group_by(postcode)%>%
  mutate(mean_tax= ceiling(total_tax/num_individuals))%>%
  mutate(year="2011")

#convert integert to have the same column character
tax_2011_final$year<-as.integer(tax_2011_final$year)
tax_2011_final$postcode<-as.integer(tax_2011_final$postcode)
#This introduce some NAs, which include non postcode one, let's remove them
any(is.na(tax_2011_final$postcode))
sum(is.na(tax_2011_final$postcode))
tax_2011_final<-na.omit(tax_2011_final)

#create immuni data for 2011 only
immu_2011<-immu_clean_raw%>%
  filter(year==2011)

#Join data immuni_2011 with tax_2011_final
combine.2011<-left_join(immu_2011,tax_2011_final,by=c("year","postcode"))
#####END 2011 ####
##let's combine them
test1 <- rbind(combine.2016,combine.2015)
test2 <- rbind(test1,combine.2014)
test3 <- rbind(test2,combine.2013)
test4 <-rbind(test3,combine.2012)
test5 <- rbind(test4,combine.2011)
write.csv(test5,"immunization_with_everything_taxation_update.csv")
