##Ordinal regression modelling
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)
library(glmnet)
#import data from ED's final merge which includes immunisation coverage, postcode, PHN, Electorate, SEIFA deciles and ranks and mean taxable income per postcode
everything <- read.csv("../cleaned_data/immunization_with_everything_taxation_update.csv")
mergeddata <- everything %>%
  select('state.x', 'postcode', 'year', 'age', 'pc_immun_class', 'PHN_number','Usual.resident.population','Rank.within.Australia...Decile', 'Electoral.division', 'mean_tax')
#selected these variables to mix postcode, age, immunisation classes, PHN, usual population, SEIFA rank within Australia in deciles, SEIFA score, Electorate and party details and mean tax of postcode.
#turn categorical variables into factors
mergeddata$postcode <- as.factor(mergeddata$postcode)
mergeddata$year <- as.factor(mergeddata$year)
mergeddata$age <- as.factor(mergeddata$age)
mergeddata$pc_immun_class <- as.factor(mergeddata$pc_immun_class)
#cleaning the data
#Filter on age
#remove pc_immun(duplicates pc_immun_class)
#remove score (duplicates Rank.withn.Australia ... Decile)
#remove PartyNms but keep electorate
#discard caution for now
#NPs are useless to us so let's remove them, this means all class 0 won't be in this model
mergeddata <- mergeddata %>%
  filter(pc_immun_class != 0)
#split test and train 60:40
inTrain <- createDataPartition(y = one_year$pc_immun_class, p = .60, list = FALSE)
training <- one_year[inTrain,]
testing <- one_year[-inTrain,]
#use 3000 rows of data as training to stop computer timing out while we build a model
experiment <- head(training, 3000)
#Ordinal regression model - because pc_immun_class has multiple categories and they have a specific order
library(MASS)
model1 <- polr(pc_immun_class ~ Rank.within.Australia...Decile + mean_tax_thousands, data = training, Hess = TRUE)
summary(model1)
coefs <- coef(model1)
coefs
# Find the p-value for model 1's t-value of 13.545
pt(13.545, 400-3, lower.tail=FALSE)*2
model2 <- polr(pc_immun_class ~ Rank.within.Australia...Decile + mean_tax_thousands + PHN_number, data = training, Hess = TRUE)
summary(model2)
coefs <- coef(model2)
coefs
#Model 1 has AIC of 79214.05 and p-value of 1.259296e-34 and Model 2 has AIC of 79211.03