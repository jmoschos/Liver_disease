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
## Convert name of target variable to y and change its value to binary

raw_data<-raw_data%>%
  rename(y=Dataset)%>%
  mutate(y=ifelse(y==2,0,y))  ## Changing target variable to 0,1 {0 = No disease, 1 = Liver disease}  
                              ## In original file its encoded {1=disease, 2 = No disease}

## Checking that the conversion is done corectly: We need 416 patients with liver disease (y=1) and 167 without (y=0)
sum(raw_data$y==0)
sum(raw_data$y==1)


## Checking the class of the variables:
str(raw_data)

## Making the y and gender variables into factors (for gender, we also change it to a binary variable)
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
train_index<-createDataPartition(raw_data$y, p = 0.9, times = 1, list= FALSE)

## Creating the subsets for train and test
train<-raw_data[train_index,]
test<-raw_data[-train_index,]

## Removing the index. No longer required.
rm(train_index)















# Exploratory data analysis

## Lets examine our target variable y

raw_data%>%
  ggplot(aes(y))+
  geom_bar()+
  labs(title = "Liver disease prevalence",
       x = "Disease",
       y= "Number of people")+
  scale_x_discrete(labels = c("No disease", "Liver disease"))


mean(raw_data$y==0)     ##Under-represented No liver disease ~28%.



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
## Lets look at percentage data instead, for comparison between the two groups.


raw_data%>%
  group_by(Gender,y)%>%
  ggplot(aes(x=Gender,fill=y))+
  geom_bar(position="fill")+
  labs( title = "Prevalence of disease based on gender",
        x = "Gender",
        y = "Percentage of instances")+
  scale_fill_discrete(name = "Category", labels = c("No disease", "Liver disease"))+
  scale_x_discrete(labels = c("Male","Female"))


## It appears that females (while being under-represented, have a slightly lower disease to no-disease ratio)

## Lets now examine the age distribution in our dataset.

raw_data%>%
  ggplot(aes(Age))+
  geom_histogram(col="blue", fill = "yellow")+
  labs( title = "Age Distribution",
        x = "Age",
        y = "Number of people")

## Age is a rather wide distribution (0-90 year old) with more observations centered around the mean

## Lets check how the age distribution differs for patients and non-patients

raw_data%>%
  ggplot(aes(x=Age, fill = y))+
  geom_histogram(col="black")+
  labs( title = "Age Distribution vs. Disease",
        x = "Age",
        y = "Number of people")+
  scale_fill_discrete(name = "Category", labels = c("No disease", "Liver disease"))

## We observe a similar distribution based on age.  



## For the rest 8 variables, we do not have domain knowledge; To explore the data, we will try to see if there are any obvious cut-off points in any of the variables that will help us predict the liver disease:

## Scatter plots with jitter:

##Function that based on an index i produces the required plot
plotVar<-function(i){
  
  ggplot(aes(x=raw_data[,i],y=y),data=raw_data)+
    geom_point(position="jitter")+
    labs( x = names(raw_data[i]))
}

## i from 1 to max column - 1 (exl the target variable)
i<-1:(ncol(raw_data)-1)

## Creating the plots and saving them to plots
plots<-map(i,plotVar)

## arranging them in a grid
ggarrange(plotlist=plots,ncol=4, nrow=3)

## removing plots
rm(plots)

##Histograms: We exclude the Gender variable that is discrete (factor)

plotHist<-function(i){
  
  ggplot(aes(x=raw_data[,i],fill=y),data=raw_data)+
    geom_histogram(col="yellow")+
    labs( x = names(raw_data[i]))
}


## index from 1 to max column -1 EXCLUDING gender variable (i==2)
i<-c(1,3:(ncol(raw_data)-1))

## Creating the plots
plots<-map(i,plotHist)

##Arranging the plots
ggarrange(plotlist=plots,ncol=3, nrow=3)

##removing the plots
rm(plots)


## Histograms deep dive: for each variable i (excl. gender) plot histogram for y==0 and y==1 next to each other

PlotHistN<-function(i){
  
p0<-raw_data%>%
  filter(y==0)%>%                                     ##filter for no disease
  ggplot(aes_string(names(raw_data[i])))+             ## Use string for aesthetic based on index i
  geom_histogram(fill="coral",col="black")+
  labs( x = names(raw_data[i]),
        y = "Number of people",
        title = "No disease")+
  ylim(0,sum(raw_data$y==1))+
  xlim(min(raw_data[,i]),1000)
  
p1<-raw_data%>%
  filter(y==1)%>%
  ggplot(aes_string(names(raw_data[i])))+
  geom_histogram(fill="light blue",col="black")+
  labs( x = names(raw_data[i]),
        y = "Number of people",
        title = "Disease")+
  ylim(0,sum(raw_data$y==1))

plot<-ggarrange(ncol=2,nrow=1,p0,p1)

  
}

i<-c(1,3:(ncol(raw_data)-1))
plots<-map(i,PlotHistN)


plots[[5]]



i<-1
raw_data%>%
  filter(y==0)%>%                                     ##filter for no disease
  ggplot(aes_string(names(raw_data[i])))+             ## Use string for aesthetic based on index i
  geom_histogram(fill="coral",col="black")+
  labs( x = names(raw_data[i]),
        y = "Number of people",
        title = "No disease")+
  ylim(0,sum(raw_data$y==1))+
  xlim(min(raw_data[,i])-1,max(raw_data[,i]+1))





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

##Default
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgb_model <- xgb.train(
  data = xgb_train, 
  params = params, 
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
  ntreelimit = 5000
)



y1<-ifelse(predictions>0.5,"1","0")
y1<-as.factor(y1)


test$y<-as.factor(test$y)

confusionMatrix(y1,test$y)



confusionMatrix(y1,test$y)
rm(xgb_test,xgb_train,predictions)


