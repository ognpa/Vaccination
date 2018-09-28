## Merging Electoral data and Postcode data for AT2

library(tidyverse)
library(readxl)

## First the Postcode by electorate file from APH
APH_post_elec <- read_excel("Postcode by Electorate all AUS - fixed.xlsx")
glimpse(APH_post_elec)
Error: `path` does not exist: ‘Postcode by Electorate all AUS - fixed.xlsx’

## wants pathway
Postcode_by_Electorate_all_AUS_fixed <- read_excel("raw_data/Postcode by Electorate all AUS - fixed.xlsx")
View(Postcode_by_Electorate_all_AUS_fixed)

##Now the AEC results for the 2016 federal election
HouseMembersElectedDownload_20499_1_ <- read_excel("raw_data/HouseMembersElectedDownload-20499 (1).xls")
View(HouseMembersElectedDownload_20499_1_)

## needs a less cumbersome title
AEC_2016_fed <- HouseMembersElectedDownload_20499_1_

## The data has a 'title' row above the data header row that is throwing out all the data, let's skip it
AEC_2016_fed <- HouseMembersElectedDownload_20499_1_(skip=1, head = TRUE)

## time to merge the two files using `leftjoin`
elec_result_PC <- APH_post_elec %>% left_join(AEC_2016_fed, by = "DivisionNm")

## it didn't like that, looks like I need to tell it the variable name in both files so it knows where to match
elec_result_PC <- APH_post_elec %>% left_join(AEC_2016_fed, by = c("Electoral division" = "DivisionNm"))
elec_result_PC

## done, time to load to github and print to csv
write.csv(elec_result_PC, "elec_result_PC.csv")
