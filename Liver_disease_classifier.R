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
if(!require(polycor)) install.packages("polycor", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(varhandle)) install.packages("varhandle", repos = "http://cran.us.r-project.org")



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
library(polycor)
library(nnet)
library(varhandle)


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
  mutate(y=as.factor(y))%>%
  mutate(Gender=ifelse(Gender=="Male","0","1"))%>%
  mutate(Gender=as.factor(Gender))


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


mean(raw_data$y==1)     ##Underepresented liver disease 28%.



## Does gender affect the disease?

raw_data%>%
  group_by(Gender,y)%>%
  ggplot(aes(x=Gender,fill=y))+
  geom_bar()+
  labs( title = "Prevalence of disease based on gender",
        x = "Gender",
        y = "Number of instances")+
  scale_fill_discrete(name = "Category", labels = c("No disease", "Liver disease"))+
  scale_x_discrete(labels = c("Male","Female"))

## Its hard to compare between the two categories, but females represent a lower percentage in our dataset.
## Lets look at percentage data instead for comparison between the two groups.


raw_data%>%
  group_by(Gender,y)%>%
  ggplot(aes(x=Gender,fill=y))+
  geom_bar(position="fill")+
  labs( title = "Prevalence of disease based on gender",
        x = "Gender",
        y = "Percentage of instances")+
  scale_fill_discrete(name = "Category", labels = c("No disease", "Liver disease"))+
  scale_x_discrete(labels = c("Male","Female"))


## It appears that females (while being under-represented, have a slightly higher disease to no-disease ratio)


## Lets now examine the age distribution in our dataset.

raw_data%>%
  ggplot(aes(Age))+
  geom_histogram(col="blue", fill = "yellow")+
  labs( title = "Age Distribution",
        x = "Age",
        y = "Number of people")

raw_data%>%
  ggplot(aes(x=Age, fill = y))+
  geom_histogram(col="black")+
  labs( title = "Age Distribution vs. Disease",
        x = "Age",
        y = "Number of people")+
  scale_fill_discrete(name = "Category", labels = c("No disease", "Liver disease"))

##Similar distribution based on age.





raw_data%>%
  ggplot(aes(x=Direct_Bilirubin, y=y))+
  geom_point()


raw_data%>%
  ggplot(aes(x=Alkaline_Phosphotase, y=y))+
  geom_point()


raw_data%>%
  ggplot(aes(x=Alamine_Aminotransferase, y=y))+
  geom_point()


raw_data%>%
  ggplot(aes(x=Aspartate_Aminotransferase, y=y))+
  geom_point()

raw_data%>%
  ggplot(aes(x=Total_Protiens, y=y))+
  geom_point()


raw_data%>%
  ggplot(aes(x=Albumin, y=y))+
  geom_point()

raw_data%>%
  ggplot(aes(x=Albumin_and_Globulin_Ratio, y=y))+
  geom_point()




## Correlation matrix: Target variable is a factor with 2 levels --> we can use the biserial correlation
i<-1:10

correlation<-sapply(i, function(i){
  if (colnames(raw_data[i])!="Gender")
  {
    polyserial(raw_data[,i],raw_data[,ncol(raw_data)])
  }
  else 0
})


fitnn<-train(y~.,data=train,method="nnet")
y_hat_nn<-predict(fitnn,test)
confusionMatrix(y_hat_nn,test$y)


fitada<-train(y~.,data=train,method="adaboost")
y_hat_ada<-predict(fitada,test)
confusionMatrix(y_hat_ada,test$y)



## XGBOOST

## Prepare the training dataset and test set

## XG boost works with numeric. We convert the factors to numerics for this algorithm only.

train$Gender<-unfactor(obj=train$Gender)
train$y<-unfactor(obj=train$y)

test$Gender<-unfactor(obj=test$Gender)
test$y<-unfactor(obj=test$y)


## We create xgb train, test and CV sets.

xgb_train <- xgb.DMatrix(
  as.matrix(train[, colnames(train) != "y"]), 
  label = (train$y))

xgb_test<-xgb.DMatrix(
  as.matrix(test[, colnames(test) != "y"]), 
  label = (test$y))

xgb_params <- list(
  objective = "binary:logistic", 
  eta = 0.1, 
  max.depth = 8, 
  nthread = 6, 
  eval_metric = "auc"
)


xgb_model <- xgb.train(
  data = xgb_train, 
  params = xgb_params, 
  watchlist = list(test = xgb_test), 
  nrounds = 5000, 
  early_stopping_rounds = 40, 
  print_every_n = 20
)

feature_imp_xgb <- xgb.importance(colnames(train), model = xgb_model)

xgb.plot.importance(feature_imp_xgb, rel_to_first = TRUE, xlab = "Relative importance")

# Make predictions based on this model

predictions = predict(
  xgb_model, 
  newdata = as.matrix(test[, colnames(test) != "y"]), 
  ntreelimit = 500
)

pred <- prediction(
  as.numeric(as.character(predictions)), as.numeric(as.character(test$y))
)

y1<-ifelse(predictions>0.5,"1","0")
y1<-as.factor(y1)
test$y<-as.factor(test$y)

confusionMatrix(y1,test$y)
rm(xgb_test,xgb_train,predictions)
