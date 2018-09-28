#Merging Postcode and PHN from 3 datasets & cleaning data
#load libraries
library(readr)
library(ggplot2)
library(dplyr)
# Import main dataset from AIHW, showing immunisation % coverage by postcode by financial years from 2011-12 to 2016-17  
immunisations_raw <- read_csv("../raw_data/AIHWpostcodesallyears.csv")
#We should now start transforming some of the data to get a visual on all years - you'll see all the data has come in as character, which isn't helpful.
glimpse(immunisations_raw)
# Clean raw immunisation data using function 

# Parameters
# * imm_data - the source data frame we want to clean

# Returns - a data frame

clean_immunisation_data <- function(imm_data) {
  #Select only the columns we need
  imm_clean = imm_data %>%
    select('State', 'Postcode', 'Reporting Year', 'Age group', 'Percent fully immunised (%)', 'Interpret with caution  (#)')
  
  #Rename column names to State, Postcode, Year, Age, pc_immun and caution
  imm_clean = rename(imm_clean, postcode = "Postcode", state = "State", year = 'Reporting Year', age = 'Age group', pc_immun ='Percent fully immunised (%)', caution = 'Interpret with caution  (#)')
  
  #Age has an unnecessary " year" in text at the end of each entry. Remove everything after the first space so we can treat age as a number, rather than character. 
  imm_clean$age = sub(" .*", "", imm_clean$age)
  #Convert Age to integer
  imm_clean$age = as.integer(imm_clean$age)
  
  #Year is poorly formatted with either '2016-2017' format or '2016 <d0> 2017' format. Just take the first 4 characters to represent year.
  imm_clean$year = substr(imm_clean$year, 1, 4)
  #Convert Year to integer format
  imm_clean$year = as.integer(imm_clean$year)
  
  #Replace caution values, so # becomes 1 and NA becomes 0. 
  imm_clean$caution[is.na(imm_clean$caution)] <- 0
  imm_clean$caution[imm_clean$caution == "#"] <- 1
  #Convert caution to integer format
  imm_clean$caution = as.integer(imm_clean$caution)
  
  #If there are empty rows (determined by rows without a postcode), remove them
  imm_clean = subset(imm_clean, !is.na(imm_clean$postcode))
  
  #Look at the unique state names.
  imm_states = imm_clean %>%
    group_by(state) %>%
    summarize(mean_age = mean(age, na.rm = TRUE)) %>%
    select(state)
  
  #There's some values that mean the same but are typed differently (duplicates). Let's clean those up
  
  #First, replace any whitespace characters in state names to reduce mistyped names. 
  imm_clean$state = gsub("[[:blank:]]","", imm_clean$state)
  
  #Fix up a couple of synonyms
  imm_clean$state[imm_clean$state == "ACT/NSW"] <- "NSW/ACT"
  imm_clean$state[imm_clean$state == "NT/SA/WA"] <- "SA/WA/NT"
  
  #Look at the unique state names again. That's better.
  imm_states = imm_clean %>%
    group_by(state) %>%
    summarize(mean_age = mean(age, na.rm = TRUE)) %>%
    select(state)
  
  return(imm_clean)
  
}# Create class numbers for immunisation perecentage groupings
# * We need to transform percent fully immunised into an extended dummy variable "Immun_class" of 9 different classses (including one for no percentage or 'NP').
# * 0 = NP, 1 = < 70%, 2 = Between 70-74.9%, 3 = Between 75-79.9%, 4 = Between 80-84.9%, 5 = Between 85-89.9%, 6 = Between 90-92.4%, 7 = Between 92.5-94.9%, 8 = 95-100%
# * Make a data frame that assigns a number to each range of percent fully immunised

# Parameters
# * imm_data - the source data frame we want to get the percent fully immunised ranges from

# Returns - a data frame

immunisation_groups <- function(imm_data) {
  #Group the data by pc_immun value to get a list of unique pc_immun values.
  #Age mean is just used as a summartize and get the unique pc_immun values, it is not a valid figure and will be discarded.
  #Select only pm_immun column and order by pc_immun in ascending order
  imm_grp <- imm_data %>%
    group_by(pc_immun) %>%
    summarize(mean_age = mean(age, na.rm = TRUE)) %>%
    select(pc_immun) %>%
    arrange(pc_immun)
  
  #Add a column of integers to respresent the pc_immun group
  imm_grp$Immun_class <- c(1:nrow(imm_grp))
  #Change the 'NP' Immun_class to 0
  imm_grp$Immun_class[imm_grp$pc_immun == "NP"] <- 0
  
  return(imm_grp)
  
}# Add immunisation class numbers to the immunisation data records

# Parameters
# * imm_data - the source data frame of immunisation data we want to add classes to
# * group_data - the source data frame of immunisation groups and class values

# Returns - a data frame

add_immunisation_class <- function(imm_data, group_data) {
  #Create a new empty data frame to hold our final clean data (we're going to add 3 extra columns, hence the +3)
  output = data.frame(matrix(ncol = ncol(imm_data)+3, nrow = 0))
  
  #Give the new data frame the same column names as our imm_clean data, with added columns for pc_immun_class, PHN code and PHN Number
  names(output) = c(colnames(imm_data),c("pc_immun_class", "PHN_code", "PHN_number"))
  
  #For each row in our group_data data frame
  for (class in group_data$Immun_class) {
    #1. Filter our imm_clean data by the pc_immun value in the current group_data row.
    temp_data = filter(imm_data, pc_immun == group_data$pc_immun[group_data$Immun_class == class])
    #If we have any data
    if (nrow(temp_data) > 0) {
      #Set the pc_immun_class in the filtered data set to the corresponding immun_class value in the Immun_grp data frame.
      temp_data$pc_immun_class = group_data$Immun_class[group_data$Immun_class == class]
      #We don't have data for these yet so set them to NA
      temp_data$PHN_code = NA
      temp_data$PHN_number = NA
      #Bind the rows to the dataframe 'output' we created earlier
      output = rbind(output, temp_data)
    }
  }
  
  #Finally, ensure immun_class is an integer
  output$pc_immun_class <- as.integer(output$pc_immun_class)
  
  return(output)
}
# Merge PHN codes into immunisation data
# * Gets the records in the phn_data that have the same postcode as the current imm_data record.
# * As we may get multiple rows because a postcode can have mutliple PHN codes associated to it, order rows from highest to lowest RATIO figure (i.e. the PHN code with the highest ratio of the population in that postcode).
# * Associate the PHN code with the highgest ratio to the imm_data record.

# Parameters
# * imm_data - the immunisation data
# * phn_data - the phn data

# Returns a data frame

merge_phn_code <- function(imm_data, phn_data) {
  
  #For each row in immunisation data
  for(i in 1:nrow(imm_data)) {
    #Filter PHN records for rows with a corresponding postcode
    PHNCodes = phn_data %>%
      filter(postcode == imm_data$postcode[i]) %>%
      arrange(desc(RATIO))
    #Get the record with the highest ratio and add its PHN code for the imm_data record 
    imm_data$PHN_code[i] = PHNCodes$PHN_CODE[1]
    #In case we want a cleaner version, also store just the PHN_number
    imm_data$PHN_number[i] = PHNCodes$PHN_number[1]
  }
  
  return(imm_data)
  
}
#Now, clean the immunisation data, get the immunisation groups and assign immunisation classes to our immunisation data

immunisations_clean <- clean_immunisation_data(immunisations_raw)
immunisation_classes <- immunisation_groups(immunisations_clean)
immunisation_data <- add_immunisation_class(immunisations_clean, immunisation_classes)
#Get Postal Area Code to PHN data from 2017and add PHN code to our immunisation data
phn_2017 <- read_csv('../raw_data/PostalAreaCodeToPHN.csv')

#PHN sometimes has 3 digit postcodes while Immunisation_data adds a leading 0 to 3 digit postcodes.
#Therefore, we add a field to PHN data called 'postcode' which will add a leading zero to any 3 digit postcode
#All PHN codes are preceeded with 'PHN' at the start of them e.g. PHN701. Let's make another column with just the number called PHN_number.
phn_2017 <- phn_2017 %>%
  mutate(postcode = ifelse(nchar(POA_CODE_2016) == 3, paste("0", POA_CODE_2016, sep=""),POA_CODE_2016)) %>%
  mutate(PHN_number = sub("PHN", "", PHN_CODE_2017)) %>%
  rename(PHN_CODE = "PHN_CODE_2017")

immunisation_data_2017 <- filter(immunisation_data, year >= 2016)
# Beware, this will take a couple of minutes!
immunisation_data_2017 <- merge_phn_code(immunisation_data_2017, phn_2017)
#Get Postal Area Code to PHN data and add PHN code to our immunisation data
phn_2011_2015 <- read_csv('../raw_data/PostalAreaCodeToPHN2011to2015.csv')

#PHN sometimes has 3 digit postcodes while Immunisation_data adds a leading 0 to 3 digit postcodes.
#Therefore, we add a field to PHN data called 'postcode' which will add a leading zero to any 3 digit postcode
#All PHN codes are preceeded with 'PHN' at the start of them e.g. PHN701. Let's make another column with just the number called PHN_number.
phn_2015 <- phn_2011_2015 %>%
  mutate(postcode = ifelse(nchar(POA_CODE_2011) == 3, paste("0", POA_CODE_2011, sep=""),POA_CODE_2011)) %>%
  mutate(PHN_number = sub("PHN", "", PHN_CODE))

immunisation_data_2015 <- filter(immunisation_data, year < 2016)

# Beware, this will take a couple of minutes!
immunisation_data_2015 <- merge_phn_code(immunisation_data_2015, phn_2015)
immunisation_data <- rbind(immunisation_data_2015, immunisation_data_2017)

#this is to write the cleaned data into the Vaccination project for us to share in Github
write_csv(immunisation_data, "../cleaned_data/immunisation_data.csv")
