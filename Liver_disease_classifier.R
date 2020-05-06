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



# Dataset loading/generation

##Data reading from my git repository jmoschos

raw_data<-read.csv("https://raw.githubusercontent.com/jmoschos/Pokemon/master/pokemon_data.csv")

## Data splitting 80% for train and 20% for test

## Making an index for the train set.
train_index<-createDataPartition(raw_data$status, p = 0.8, times = 1, list= FALSE)

## Creating the subsets for train and test
train<-raw_data[train_index,]
test<-raw_data[-train_index,]

## Removing the index. No longer required.
rm(train_index)

