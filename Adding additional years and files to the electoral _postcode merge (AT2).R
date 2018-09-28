## The group agreed we needed multiple years of data in order to train our model later, so I will now be merging the 2010 and 2013 federal election data, with the already merged, postcode and electoral result (2016) data.

library(tidyverse)
library(readxl)

## load 2013 federal election data and skip row 1 as per 2016 datafile
AEC_2013_fed <- read_csv("raw_data/HouseMembersElectedDownload-2013.csv",
                         skip = 1)
View(AEC_2013_fed)

## Now do the same for 2010

AEC_2010_fed <- read_csv("raw_data/HouseMembersElectedDownload - 2010.csv",
                         skip = 1)
## As the data files all have the same variable names acros 2010, 2013 and 2016, before I merge the data, I want to make sure I can distinguish between years, so I rename the columns to include the year for each variable. I'm not sure what we will need for EDA yet, so I want to keep all columns in for the merge. 

colnames(AEC_2013_fed)[colnames(AEC_2013_fed)=="PartyNm"] <- "PartyNm2013"
colnames(AEC_2013_fed)[colnames(AEC_2013_fed)=="PartyAb"] <- "PartyAb2013"
colnames(AEC_2013_fed)[colnames(AEC_2013_fed)=="GivenNm"] <- "GivenNm2013"
colnames(AEC_2013_fed)[colnames(AEC_2013_fed)=="Surname"] <- "Surname2013"
colnames(AEC_2013_fed)[colnames(AEC_2013_fed)=="CandidateID"] <- "CandidateID2013"
colnames(AEC_2013_fed)[colnames(AEC_2013_fed)=="DivisionID"] <- "DivisionID2013"
colnames(AEC_2013_fed)[colnames(AEC_2013_fed)=="StateAB"] <- "StateAB2013"
colnames(AEC_2013_fed)[colnames(AEC_2013_fed)=="DivisionNm"] <- "DivisionNm2013"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="DivisionNm"] <- "DivisionNm2010"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="DivisionID"] <- "DivisionID2010"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="StateAB"] <- "StateAB2010"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="CandidateID"] <- "CandidateID2010"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="GivenNm"] <- "GivenNm2010"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="Surname"] <- "Surname2010"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="PartyMm"] <- "PartyNm2010"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="PartyAb"] <- "PartyAb2010"
colnames(AEC_2010_fed)[colnames(AEC_2010_fed)=="PartyNm"] <- "PartyNm2010"

## There has to be a quicker way to do this than one by one, but I'll figure it out later

## Now the columns have been renamed, I am going to use `leftjoin` just like I did with the 2016 file, to bring in the 2013 results data
elec_result_PC_all <- elec_result_PC %>% left_join(AEC_2010_fed, by = c("Electoral division" = "DivisionNm2010"))
View(elec_result_PC_all)

## Agin, I'm sure there's a way to merge multiple files at once, but I'm just going to stick with what I know works for now

elec_result_PC_2010_2016 <- elec_result_PC_all %>% left_join(AEC_2013_fed, by = c("Electoral division" = "DivisionNm2013"))
View(elec_result_PC_2010_2016)

## Opps forgot to change the 2016 column names, now they are out of naming convention with the new columns, this was ok when there was only 1 year of data, but need to change them:

colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="DivisionID"] <- "DivisionID2016"
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="StateAb.x"] <- "StateAB2016"
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="CandidateID"] <- "CandidateID2016"
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="GivenNm"] <- "GivenNm2016"
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="GivenNm"] <- "GivenNm2016"
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="Surname"] <- "Surname2016"
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="PartyNm"] <- "PartyNm2016"
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="PartyAb"] <- "PartyAb2016"

## and View has shown me that I missed a few columns in the first pass. 

colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="StateAb.y"] <- "StateAb2010"
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="StateAb"] <- "StateAb2013"

## Rename the percent column so It's clear what it's a percent of
colnames(elec_result_PC_all)[colnames(elec_result_PC_all)=="Per cent"] <- "Per cent postcode in electorate"

## Load updated merge file into Github rep and print to csv.
write_csv(elec_result_PC_all, "elec_results_PC_all.csv")
