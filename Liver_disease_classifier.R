# Final Project: Harvard Data science professional program
# May 2020

# Library Installation

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")



# Library loading


library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(randomForest)
library(rpart)
library(xgboost)
library(ggplot2)
library(stringr)
library(ggpubr)
library(tinytex)
library(kableExtra)
library(httr)
library(RCurl)
library(ROCR)



# Dataset loading

##Automatic Data reading from my git repository jmoschos

raw_data<-read.csv("https://raw.githubusercontent.com/jmoschos/Liver_disease/master/indian_liver_patient.csv")


# Basic manipulations
## Convert name of target variable to y

raw_data<-raw_data%>%
  rename(y=Dataset)%>%
  mutate(y=y-1)             ##Changing target variable to 0,1 {0 = No disease, 1 = Liver disease}

## Checking the class of the variables:
str(raw_data)

## Making the y variable into a factor:
raw_data<-raw_data%>%
  mutate(y=as.factor(y))


## Missing values ? 

sapply(raw_data, function(x) sum(is.na(x)))       ## only albumin and globulin ration has some NA's 

## We will replace the NAs with the mean of the rest of the observations.If there were more outliers, a better choice might have been the median.

raw_data$Albumin_and_Globulin_Ratio<-ifelse(is.na(raw_data$Albumin_and_Globulin_Ratio), mean(raw_data$Albumin_and_Globulin_Ratio,na.rm=TRUE),raw_data$Albumin_and_Globulin_Ratio)     ## if the value is NA, replace it with the mean of the rest of the values (NAs excluded with na.rm argument, otherwise keep it as is)



# Dataset generation
## Data splitting 80% for train and 20% for test

## Making an index for the train set.
train_index<-createDataPartition(raw_data$y, p = 0.8, times = 1, list= FALSE)

## Creating the subsets for train and test
train<-raw_data[train_index,]
test<-raw_data[-train_index,]

## Removing the index. No longer required.
rm(train_index)



# Exploratory data analysis

## Lets examine our target variable y

raw_data%>%
  ggplot(aes(y))+
  geom_bar()











