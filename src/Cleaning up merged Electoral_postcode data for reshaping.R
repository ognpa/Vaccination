##removing some un-needed columns in the data before trying to reshape the file
elec_result_PC_all <- select(elec_result_PC_all, -startsWith("DivisionID"))
Error in select(elec_result_PC_all, -startsWith("DivisionID")) : 
  could not find function "select"

## ok better look it up
## realise I hadn't reloaded the tidyverse library since my computer died
library(tidyverse)
── Attaching packages ──────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
✔ tibble  1.4.2     ✔ dplyr   0.7.6
✔ purrr   0.2.5     ✔ stringr 1.3.1
✔ tibble  1.4.2     ✔ forcats 0.3.0
── Conflicts ─────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()

library(tidyselect)
library(tidyr)

elec_result_PC_all <- select_(elec_result_PC_all, -startsWith("DivisionID"))
Error in startsWith("DivisionID") : 
  argument "prefix" is missing, with no default

## ok let's try that again - `starts_with` needed the underscore
elec_result_PC_all <- select(elec_result_PC_all, -starts_with("DivisionID"))

##ok good, now for more columns
elec_result_PC_all <- select(elec_result_PC_all, -starts_with("DivisionID", "Surname", "GivenNm", "PartyAB", "CandidateID"))
Error in starts_with("DivisionID", "Surname", "GivenNm", "PartyAB", "CandidateID") : 
  unused arguments ("PartyAB", "CandidateID")

## ok - try 'one-of
elec_result_PC_all <- select(elec_result_PC_all, -one_of(c("DivisionID2016", "CandidateID2016")))

## that worked, but seems a bt tedious naming every single column seperately, there must be a better way, back to `starts_with`, at least that can remove colum groups

elec_result_PC_all <- select(elec_result_PC_all, -starts_with("DivisionID", "Surname", "GivenNm", "PartyAb", "CandidateId"))
Error in starts_with("DivisionID", "Surname", "GivenNm", "PartyAb", "CandidateId") : 
  unused arguments ("PartyAb", "CandidateId")

## ok, no luck, how about `contains`
elec_result_PC_all <- select(elec_result_PC_all, -contains("DivisionID", "Surname", "GivenNm", "PartyAb", "CandidateId"))
Error in contains("DivisionID", "Surname", "GivenNm", "PartyAb", "CandidateId") : 
  unused arguments ("PartyAb", "CandidateId")

## Back to `starts_with` but why does this only seem to work one by one, this will take a while
elec_result_PC_all <- select(elec_result_PC_all, -starts_with("Surname"))
elec_result_PC_all <- select(elec_result_PC_all, -starts_with("GivenNm"))
elec_result_PC_all <- select(elec_result_PC_all, -starts_with("PartyAb"))

## how about a few at once
elec_result_PC_all <- select(elec_result_PC_all, -starts_with("Surname", "GivenNm"))
Error in if (ignore.case) match <- tolower(match) : 
  argument is not interpretable as logical

## ok that didn't work, last column to clean up is state, but I want to leave one of them behind, the other 2 can go ( we don't need 3 state columns by year seeing as they don't change)
rename(elec_result_PC_all, StateAb.y = State)
Error in .f(.x[[i]], ...) : object 'State' not found

## the example in `?select` doesn't need quotations, but maybe I do?
rename(elec_result_PC_all, StateAb.y = "State")
Error: Unknown column `State` 
In addition: Warning message:
  'glue::collapse' is deprecated.
Use 'glue_collapse' instead.
See help("Deprecated") and help("glue-deprecated"). 

## this is frustrating, isn't the whole point of the rename functionm to rename things, I know that column name doesn't exist yet.. 
elec_result_PC_all <- rename_(elec_result_PC_all, "State" = "StateAB2016")

#This looks promising
View(elec_result_PC_all)

#ok, now to remove those extra state by year columns
elec_result_PC_all <- select(elec_result_PC_all, -matches("StateAb.y"))
elec_result_PC_all <- select(elec_result_PC_all, -matches("StateAb"))

##And the final piece which is the CandidateID columns
elec_result_PC_all <- select(elec_result_PC_all, -starts_with("CandidateID"))

## ok now that I have a cleaner data file in `wide` format, I'd like to convert it to `long` format
##  Make sure the postcode column is a factor
elec_result_PC_all$Postcode <- factor(elec_result_PC_all$Postcode)
elec_result_PC_all_long <- gather(elec_result_PC_all, Party_Year, Party_Name, PartyNm2016:PartyNm2013, factor_key = TRUE)
elec_result_PC_all_long

### Rename factor names from "PartyNm2016", "PartyNm2010" and "PartyNm2013" to "2016", "2010" and "2013"
levels(elec_result_PC_all_long$Party_Year)[levels(elec_result_PC_all_long$Party_Year)=="PartyNm2016"] <- "2016"
levels(elec_result_PC_all_long$Party_Year)[levels(elec_result_PC_all_long$Party_Year)=="PartyNm2013"] <- "2013"
levels(elec_result_PC_all_long$Party_Year)[levels(elec_result_PC_all_long$Party_Year)=="PartyNm2010"] <- "2010"

## Done - ready for merging with the other datasets in `long` format & with unecessary columns removed. 
