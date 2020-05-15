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
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(doSNOW)) install.packages("doSNOW", repos = "http://cran.us.r-project.org")
if(!require(fastAdaboost)) install.packages("fastAdaboost", repos = "http://cran.us.r-project.org")

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
library(reshape2)
library(e1071)
library(gbm)
library(doSNOW)
library(fastAdaboost)


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

sapply(raw_data, function(x) sum(is.na(x)))       ## only albumin and globulin ratio has some NA's 

## We will replace the NAs with the mean of the rest of the observations.If there were more outliers, a better choice might have been the median.

raw_data$Albumin_and_Globulin_Ratio<-ifelse(is.na(raw_data$Albumin_and_Globulin_Ratio), mean(raw_data$Albumin_and_Globulin_Ratio,na.rm=TRUE),raw_data$Albumin_and_Globulin_Ratio)     ## if the value is NA, replace it with the mean of the rest of the values                                                       ## (NAs excluded with na.rm argument, otherwise keep it as is)




# Dataset generation
## Data splitting: 80% for train and 20% for test

##Seed for reproducability
set.seed(1)

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
  geom_bar()+
  labs(title = "Liver disease prevalence",
       x = "Disease",
       y= "Number of people")+
  scale_x_discrete(labels = c("No disease", "Liver disease"))


mean(raw_data$y==0)     ##Under-represented No liver disease ~28.6%.



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
  labs( title = "Scaled Prevalence of disease based on gender",
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

## i from 3 (excl. gender and age) to max column - 1 (exluding the target variable)
i<-3:(ncol(raw_data)-1)

## Creating the plots and saving them to plots
plots<-map(i,plotVar)

## arranging them in a grid
ggarrange(plotlist=plots,ncol=2, nrow=4)

## removing plots
rm(plots)

##Histograms: We exclude the Gender variable that is categorical (factor)

plotHist<-function(i){
  
  ggplot(aes(x=raw_data[,i],fill=y),data=raw_data)+
    geom_histogram(col="yellow")+
    labs( x = names(raw_data[i]),
          y = "Count")+
    scale_fill_discrete(name = "Category", labels = c("No disease", "Liver disease"))
}


## index from 1 to max column -1 EXCLUDING gender variable (i==2)
i<-c(3:(ncol(raw_data)-1))

## Creating the plots
plots<-map(i,plotHist)

##Arranging the plots
ggarrange(plotlist=plots,ncol=2, nrow=4)

##removing the plots and index variable
rm(plots,i)



## Lets check the correlation between all the continuous variables

## Picking only the continuous variables
corvar<-raw_data[,c(1,3:10)]

##Creating the correlation matrix
cormatrix<-cor(corvar)

## we are going to use only the upper part of the matrix and leave the rest blank
cormatrix[upper.tri(cormatrix)]<-NA

## Using reshape library, we are "melting" the matrix into a useable form for a heatmap
cormatrix<-melt(cormatrix,na.rm = TRUE)

## Plotting the heatmap of correlation
cormatrix%>%
  ggplot(aes(x=Var1,y=Var2,fill=value))+
  geom_tile(color="white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

rm(cormatrix,corvar)




## Before doing any manipulation to the original dataset, we are going to save them. We will utilize them for benchmarking later in the process.
trainS<-train
testS<-test






## Adding a new variable: Number of extremes for blood test results

## it seems that most sick patients have values that are much more extreme than that of healthy patients. It might be interesting to explore for the sick patients, how many values of the continuous variables are outside the normal range (normal range is defined as the range observed in healthy patients.)

##dummy variable = number of "normal values for other variables". If a patient has a value within the range of a normal patient y=0. In the end we sum the values of these 8 dummy variables for the total number of out-of-bounds variables.



## Find the min and max for healthy patients     

## the range is calculated ONLY ON THE TRAIN DATA. We do not use the unseen data for our calculations

min<-apply(train[train$y==0,],2,min)
max<-apply(train[train$y==0,],2,max)

## Make these limits into 1 dataframe
lims<-data.frame(min=min,max=max)

##removing min and max
rm(min,max)

##Removing age,gender and target variable   --> We only care for the blood results
lims<-lims[3:10,]     

##Making the lims into numeric (and not factor)
lims<-unfactor(lims) 

##Function to create the 8 dummy variables per patients. Checking if the reading is less than min of a healthy patients or more than the max of a healthy patient. If it is outside the bounds 1, otherwise 0.

y<-function(i,data){ifelse(data[,i+2]<lims$min[i] | data[,i+2]>lims$max[i],1,0)}

## Index i for 1 to number of rows (i.e. variables) in lims ==8
i<-1:nrow(lims)
d_train<-sapply(i,y,data=train)       ##calculating the new variables for the train set
d_test<-sapply(i,y,data=test)         ## Calculating the new variables for the test set


##Calculating the total number of "normal values" for any given patient
d_train<-rowSums(d_train)
d_test<-rowSums(d_test)


## Binding the original datasets with the new variable we created
train<-cbind(train,d_train)
test<-cbind(test,d_test)
train<-train%>%
  rename(Number_of_extremes=d_train)

test<-test%>%
  rename(Number_of_extremes=d_test)


## Removing the un-needed variables
rm(d_test,d_train,i,lims)


## Histograms for number of extremes in healthy and sick individuals:

## In the train set:

p0<-train%>%
  filter(y==0)%>%
  ggplot(aes(Number_of_extremes))+
  geom_histogram()+
  xlim(-1,6)

p1<-train%>%
  filter(y==1)%>%
  ggplot(aes(Number_of_extremes))+
  geom_histogram()+
  xlim(-1,6)


## Arranging the plots
rm(p0,p1)

p0<-test%>%
  filter(y==0)%>%
  ggplot(aes(Number_of_extremes))+
  geom_histogram()+
  xlim(-1,6)

p1<-test%>%
  filter(y==1)%>%
  ggplot(aes(Number_of_extremes))+
  geom_histogram()+
  xlim(-1,6)

## Arranging the plots
ggarrange(p0,p1,ncol=2,nrow=1)

## removing the subplots
rm(p0,p1)



## Feature selection

## Some variables are highly correlated with each other. We are going to select the variables with the minimum variability and remove them from our sets for the model training.

## Examining Total vs. Direct Bilirubin

p1<-train%>%
  ggplot(aes(x="",y=train[,3]))+
  geom_boxplot()+
  geom_point(position="jitter")+
  labs(title="Total Bilirubin")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ylim(0,80)

p2<-train%>%
  ggplot(aes(x="",y=train[,4]))+
  geom_boxplot()+
  geom_point(position="jitter")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ylim(0,80)+
  labs(title="Direct Bilirubin")

## Arranging in 1 plot
ggarrange(p1,p2,ncol=2,nrow=1)

## Removing subplots
rm(p1,p2)

## Total bilirubin has a higher variation, so we will keep this


## Examining Alamine aminotransferase vs. Aspartate aminostransferase

p1<-train%>%
  ggplot(aes(x="",y=train[,6]))+
  geom_boxplot()+
  geom_point(position="jitter")+
  labs(title="Alamine aminotransferase ")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
ylim(0,5000)

p2<-train%>%
  ggplot(aes(x="",y=train[,7]))+
  geom_boxplot()+
  geom_point(position="jitter")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ylim(0,5000)+
  labs(title=" Aspartate aminostransferase")

## arranging in 1 plot for comparison
ggarrange(p1,p2,ncol=2,nrow=1)

## Removing plots
rm(p1,p2)

## Aspartate has a higher variation, so we will remove alamine



## For albumine, since its highly correlated with both Albumin to Globulin ratio and Total proteins, we will remove it.

## Our final feature selection includes: Age, Gender, Total Bilirubin, Alkaline Phosphotase, Aspartate Aminostransferase, Total Proteins and Albumin/Globulin Ratio and the Number of extremes we created.

train<-train[,c(1,2,3,5,7,8,10,11,12)]
test<-test[,c(1,2,3,5,7,8,10,11,12)]



## Preprocessing the data: Center and scaling

preProcValues <- preProcess(train, method = c("center", "scale"))

## Calculate thew new centered/scaled values
trainTransformed <- predict(preProcValues, train)
testTransformed <- predict(preProcValues, test)

## Set the train and test set equal to the processed data.
train<-trainTransformed
test<-testTransformed





## Model building

## In ALL the models, we have set the seed to 1 for reproducability.

## Control = 10-fold cross validation

control<-trainControl(method="cv",number=10)

## Metric = Accuracy
metric="F1"


## Naive model - Always liver disease

y_hat_naive<-rep.int(1,times=nrow(test))
y_hat_naive<-factor(y_hat_naive, levels=c("0","1"))
confusionMatrix(y_hat_naive,test$y)$overall["Accuracy"]

Res_Naive<-data.frame(method="Naive",Accuracy=confusionMatrix(y_hat_naive,test$y)$overall["Accuracy"])

F_Naive<-data.frame(method="Naive",F1=F_meas(y_hat_naive,test$y))

Results_Naive<-inner_join(Res_Naive,F_Naive, by = "method")

## Classification tree

set.seed(1)
grid<-expand.grid(cp=seq(0,1,0.001))     ##tuning for complexity parameter
fittree<-train(y~.,
               data=train,
               method="rpart",
               trControl=control,
               metric=metric,
               tuneGrid=grid)

y_hat_tree<-predict(fittree,test)
confusionMatrix(y_hat_tree,test$y)$overall["Accuracy"]


Res_CART<-data.frame(method="Classification tree",Accuracy=confusionMatrix(y_hat_tree,test$y)$overall["Accuracy"])

F_CART<-data.frame(method="Classification tree",F1=F_meas(y_hat_tree,test$y))

Results_CART<-inner_join(Res_CART,F_CART, by = "method")

## Neural network

set.seed(1)
fitnn<-train(y~.,
             data=train,
             method="nnet",
             trControl=control,
             metric=metric)
y_hat_nn<-predict(fitnn,test)
confusionMatrix(y_hat_nn,test$y)$overall["Accuracy"]

Res_NN<-data.frame(method="Neural network",Accuracy=confusionMatrix(y_hat_nn,test$y)$overall["Accuracy"])

F_NN<-data.frame(method="Neural network",F1=F_meas(y_hat_nn,test$y))

Results_NN<-inner_join(Res_NN,F_NN, by = "method")

## Adaboost

set.seed(1)
grid<-expand.grid(nIter=seq(50,250,50),method="adaboost")     ##Tuning for number of trees
fitada<-train(y~.,
              data=train,
              method="adaboost",
              trControl=control,
              tuneGrid=grid,
              metric=metric)

y_hat_ada<-predict(fitada,test)
confusionMatrix(y_hat_ada,test$y)$overall["Accuracy"]

Res_ADA<-data.frame(method="Ada boost",Accuracy=confusionMatrix(y_hat_ada,test$y)$overall["Accuracy"])

F_ADA<-data.frame(method="Ada boost",F1=F_meas(y_hat_ada,test$y))

Results_ADA<-inner_join(Res_ADA,F_ADA, by = "method")

##GBM

set.seed(1)
grid<-expand.grid(n.trees=seq(50,250,50),
                  interaction.depth=1,
                  shrinkage=seq(0.1,0.5,0.1),
                  n.minobsinnode=seq(5,20,1))

gbmFit1 <- train(y ~ ., data = train, 
                 method = "gbm", 
                 trControl = control,
                 verbose = TRUE,
                 tuneGrid = grid,
                 metric=metric)

y_gbm<-predict(gbmFit1,test)
confusionMatrix(y_gbm,test$y)$overall["Accuracy"]

Res_GBM<-data.frame(method="GBM",Accuracy=confusionMatrix(y_gbm,test$y)$overall["Accuracy"])

F_GBM<-data.frame(method="GBM",F1=F_meas(y_gbm,test$y))

Results_GBM<-inner_join(Res_GBM,F_GBM, by = "method")

## Support vector machine

set.seed(1)
fitsvm<-train(y~.,
              data=train,
              method="svmRadial",
              trControl=control,
              metric=metric)
y_hat_svm<-predict(fitsvm,test)
confusionMatrix(y_hat_svm,test$y)$overall["Accuracy"]

Res_SVM<-data.frame(method="SVM",Accuracy=confusionMatrix(y_hat_svm,test$y)$overall["Accuracy"])

F_SVM<-data.frame(method="SVM",F1=F_meas(y_hat_svm,test$y))

Results_SVM<-inner_join(Res_SVM,F_SVM, by = "method")

## K-nearest neighbors

set.seed(1)
fitknn<-train(y~.,
              data=train,
              method="knn",
              tuneGrid=expand.grid(k=seq(1:100)),
              trControl=control,
              metric=metric)

y_hat_knn<-predict(fitknn,test)
confusionMatrix(y_hat_knn,test$y)$overall["Accuracy"]

Res_KNN<-data.frame(method="KNN",Accuracy=confusionMatrix(y_hat_knn,test$y)$overall["Accuracy"])

F_KNN<-data.frame(method="KNN",F1=F_meas(y_hat_knn,test$y))

Results_KNN<-inner_join(Res_KNN,F_KNN, by = "method")

## Random forest model

set.seed(1)
fitrf<-train(y~.,
             data=train,
             method="rf",
             tuneGrid=expand.grid(mtry=seq(3,20,1)),
             trControl=control,
             metric=metric)

y_hat_rf<-predict(fitrf,test)
confusionMatrix(y_hat_rf,test$y)$overall["Accuracy"]

Res_RF<-data.frame(method="Random forest",Accuracy=confusionMatrix(y_hat_rf,test$y)$overall["Accuracy"])

F_RF<-data.frame(method="Random forest",F1=F_meas(y_hat_rf,test$y))

Results_RF<-inner_join(Res_RF,F_RF, by = "method")

## XG BOOST

set.seed(1)
tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50, 75, 100),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(0.3, 0.4, 0.5),
                         gamma = 0,
                         subsample = 1)

fitxgb <- train(y ~ ., 
                  data = train,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = control,
                  metric=metric)

y_xgb<-predict(fitxgb,test)
confusionMatrix(y_xgb,test$y)$overall["Accuracy"]

Res_XGB<-data.frame(method="XGBoost",Accuracy=confusionMatrix(y_xgb,test$y)$overall["Accuracy"])

F_XGB<-data.frame(method="XGBoost",F1=F_meas(y_xgb,test$y))

Results_XGB<-inner_join(Res_XGB,F_XGB, by = "method")

##combining all results

Results<-rbind(Results_ADA,Results_CART,Results_GBM,Results_KNN,Results_Naive,Results_NN,Results_RF,Results_SVM,Results_XGB)

## removing individual dataframes

rm(Res_ADA,Res_CART,Res_GBM,Res_KNN,Res_Naive,Res_NN,Res_RF,Res_SVM,Res_XGB)
rm(F_ADA,F_CART,F_GBM,F_KNN,F_Naive,F_NN,F_RF,F_SVM,F_XGB)




## Best model = Random forest 81.03% accuracy


## Given that the best model is a Random forest, we are going to retrain our model but with the original unprocessed data (unscaled and without the extra variable we created, but with the variables we removed) to see if our features added value to the model)


## Random forest model with original dataset

set.seed(1)
fitrfS<-train(y~.,
             data=trainS,                             ## using trainS and not train dataset
             method="rf",
             tuneGrid=expand.grid(mtry=seq(3,20,1)),
             trControl=control,
             metric=metric)

y_hat_rf_S<-predict(fitrfS,testS)
confusionMatrix(y_hat_rf_S,testS$y)$overall["Accuracy"]

Res_RF_S<-data.frame(method="Random forest",Accuracy=confusionMatrix(y_hat_rf_S,testS$y)$overall["Accuracy"])

F_RF_S<-data.frame(method="Random forest",F1=F_meas(y_hat_rf_S,testS$y))

Results_RF_S<-inner_join(Res_RF_S,F_RF_S, by = "method")
