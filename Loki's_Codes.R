### Title       : STDS Assignment 2 Part B
### Author      : Lhogeshwaran Purushothaman for 'The Vaccinators'
### Student ID  : 13313491

## Clear environment
rm(list = ls())

## Install and load required libraries
library(httr)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(rvest)
library(ggplot2)
library(readr)
library(tidyverse)
library(readxl)
library(DataExplorer)
library(lubridate)
library(glmnet)

## Set working directory
setwd("/Users/loki/UTS_MDSI/Sem1/STDS/AT2/Datasets")
getwd()
list.files()

### 1) Data wrangling
## Data sourced from : ABS Webssites 

## Load datasets into R
abs_pop=read.csv("ABS_REGIONAL_pop.csv", header=TRUE)
abs_fam=read.csv("ABS_REGIONAL_family&community.csv", header=TRUE)
abs_edu=read.csv("ABS_REGIONAL_educ&employment.csv", header=TRUE)
abs_eco=read.csv("ABS_REGIONAL_economy&income.csv", header=TRUE)
vac_rates=read_excel("datasheet-report-hc42.xlsx", sheet = 'TAB 3',skip=15)

## Merge the datasets
abs_data <- rbind(abs_eco,abs_edu,abs_fam,abs_pop)
str(abs_data)
abs_data$ASGS_2016 <- as.factor(abs_data$ASGS_2016) #### This is the SA3 code for abs dataset 
str(vac_rates)
vac_rates$`SA3 code` <- as.factor(vac_rates$`SA3 code`) #### This is the SA3 code for vaccination dataset
#### SA3 codes are used as the key for merging the datasets
demo_and_vac_merged <- merge(vac_rates,abs_data,by.x='SA3 code',by.y='ASGS_2016',all.y = TRUE)
str(demo_and_vac_merged_filtered)
#### Clean the data - Rename useful columns, remove unnecessary columns 
#### For model designing, only few Data.item (demographics) are retained. This is subject to discussion with group and change per views from team.
demo_and_vac_merged <- demo_and_vac_merged %>%
  select(-c(6,7,8,10,11,13,14,16,17,18,21,22))
head(demo_and_vac_merged)
str(demo_and_vac_merged_filtered)
unique(demo_and_vac_merged$Data.item)
#### This (demo_and_vac_merged_filtered) is created to develop a model and shall be replaced after finalizing the model with'demo_and_vac_merged'.
demo_and_vac_merged_filtered <- demo_and_vac_merged[demo_and_vac_merged$Data.item %in% c("Persons earning $1000-$1999 per week (%)","Persons earning nil income (%)"),]
demo_and_vac_merged_filtered <- subset(demo_and_vac_merged_filtered,demo_and_vac_merged_filtered$`Percent fully immunised (%)` != 'NP')
demo_and_vac_merged_filtered <- subset(demo_and_vac_merged_filtered,demo_and_vac_merged_filtered$`Percent fully immunised (%)` != 'NA')

## Create actual target status as '1' for 'attained' and '0' for 'not attained' with 90% immunisation percentage as goal
demo_and_vac_merged_filtered$Immunisation_target <- as.factor(ifelse(demo_and_vac_merged_filtered$`Percent fully immunised (%)` >= 90, 1, 0))

### 2) Identify features and build a linear classiciation model to predict which SA3s are likely to attain the target (response)

##  Create train and test datasets 
train_size <- floor(.7*nrow(demo_and_vac_merged_filtered)) 
set.seed(47)
train_index <-  sample(seq_len(nrow(demo_and_vac_merged_filtered)), size = train_size)
trainset <- demo_and_vac_merged_filtered[train_index,]
testset <- demo_and_vac_merged_filtered[-train_index,]

#### Linear classification models : Ridge (alpha 0) and Lasso (alpha 1)  
#### Create x and y matriX required for glmnet package

x <- model.matrix(~ ., trainset[,])
y <- trainset$Immunisation_target

#### Lasso modelling 
cv.fit_lasso <- cv.glmnet(x, y, family = 'binomial', type.measure = 'auc', alpha = 1)

#### Coefficients
lasso_coef <- coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min)
lasso_coef

plot(cv.fit_lasso)

#### Classification using coeficients with a threshold of 50%
prediction_lasso = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[,]), 
                           type = "class",
                           s = cv.fit_lasso$lambda.min)


#### AUC 
prediction_lasso <-  as.ordered(prediction_lasso)
roc_lasso <- roc(testset$Immunisation_target, prediction_lasso)
auc(roc_lasso)
plot(roc_lasso)
#### Confusion matrix
lasso_confusion <- table(predicted = prediction_lasso, true = testset$Immunisation_target)
lasso_confusion
#### Accuracy 
lasso_accuracy <- (lasso_confusion[1,1]+lasso_confusion[2,2])/sum(lasso_confusion)
lasso_accuracy
#### Precision [TP/(TP+FP)]
lasso_precision <- lasso_confusion[2,2]/(lasso_confusion[2,2]+lasso_confusion[2,1])
lasso_precision
#### Recall [TP/(TP+FN)] 
lasso_recall <- lasso_confusion[2,2]/(lasso_confusion[2,2]+lasso_confusion[1,2])
lasso_recall
#### F1 [2*((precision*recall)/(precision+recall))]
lasso_f1 <- 2*(lasso_precision*lasso_recall/(lasso_precision+lasso_recall))
lasso_f1

#### Adjust the probability threshold to an optimum value
probability_lasso = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[,]), 
                            type = "response",
                            s = cv.fit_lasso$lambda.min)
Ltestset <- testset
Ltestset$estimated_probs <- probability_lasso 

#### Loop to test multiple values of threshold
output <- data_frame()
for(i in seq(from=.01, to=.99, by=.01)){
  Ltestset$prediction <- 0
  Ltestset[Ltestset$estimated_probs >= i, "prediction"] = 1
  
  cfm <- table(Predicted = Ltestset$prediction, True = Ltestset$Target) # auxiliary confusion matrix
  accuracy <- (cfm[1,1]+cfm[2,2])/sum(cfm) 
  precision <- cfm[2,2]/(cfm[2,2]+cfm[2,1])
  recall <- cfm[2,2]/(cfm[2,2]+cfm[1,2])
  f1 <- 2*(precision*recall/(precision+recall))
  TP <- cfm[2,2]
  FN <- cfm[1,2]0
  FP <- cfm[2,1]
  
  # AUC 
  Ltestset$prediction <- as.ordered(Ltestset$prediction)
  roc <- roc(Ltestset$Target, Ltestset$prediction)
  lasso_auc <- auc(roc)
  
  
  temp <- data_frame(i, accuracy, precision, recall, f1, lasso_auc, TP, FN, FP)
  output <- rbind(output, temp)
}

output %>%
  ggplot() +
  geom_line(aes(x = i, y = accuracy), color = "yellow", show.legend = T) +
  geom_line(aes(x = i, y = precision), color = "black") +
  geom_line(aes(x = i, y = recall), color = "blue") +
  geom_line(aes(x = i, y = lasso_auc), color = "green") +
  geom_line(aes(x = i, y = f1)) +
  theme_minimal()

######## Trial area ########

###### Trial 1 -- Start
rm(train_size) ### To remove test datasets

###### Trial 1 -- End

############################
