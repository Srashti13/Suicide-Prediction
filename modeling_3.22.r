library(tidyverse)
library(dplyr)
library(countrycode)
library(caTools)
library(randomForest)
library(reshape2)
source('hw.r')

# import the dataset and preprocess
dataset <- read.csv('master.csv')
dataset <- select(dataset,ï..country,year,sex,age, suicides.100k.pop, 
                  gdp_per_capita....)
colnames(dataset) <- c('country','year','sex','age','suicidesP','GDPC')

# add region information to the dataset 
dataset$continent <- countrycode(sourcevar = dataset[, "country"],
                            origin = "country.name",
                            destination = "continent")
dataset <- dataset[,2:7]
dataset <- dataset[,c(4,6,1,2,3,5)]
dataset$continent <- as.factor(dataset$continent)


# split the dataset into training and testing
set.seed(123)
split = sample.split(dataset$suicidesP, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
sapply(training_set, function(x) sum(is.na(x)))


#################################################################
# fit the random Forest model to the training_set
set.seed(123)
regressor <- randomForest(suicidesP ~ .,mtry=5,data=training_set,ntree=20,importance=TRUE) 
# takes a few minutes
regressor

# predict results from the test_set
y_predict <- predict(regressor,test_set[,-(1)])
y_predict

mean((y_predict-test_set[,1])^2) # 110.9041 MSE for the test set


##################################################################
## visualize the prediction and real data
ggplot()+geom_point(aes(x=test_set[,1],y=y_predict),color='blue')+
  geom_abline(aes(slope=1,intercept=0),color='red',size=0.8)+ 
  scale_x_continuous(breaks=c(0,25,50,75,100,125))+
  scale_y_continuous(breaks=c(0,25,50,75,100,125))+
  labs(title= 'Predicted suicide rates vs. real suicide rates \n(over 100k population)', 
       x='Predicted rates',
       y='Real rates')+ hw

## visualize the prediction and real data (comparison)
# change the format
test_set_update <- data.frame(test_set,y_predict)
test_set_update <- test_set_update[,c(1,7)]
test_set_update$key <- c(1:7930)
test_set_updatef <- melt(test_set_update,id='key')
test_set_updatef <- test_set_updatef[,c(2,3)]
names(test_set_updatef) <- c('pred_real','value')

# plot the comparison - overall, may add more details about it
ggplot(test_set_updatef)+geom_boxplot(aes(x= pred_real,y=value,fill=pred_real))+
  scale_y_continuous(breaks=seq(0,150,30))+
  labs(title= 'Predicted suicide rates vs. real suicide rates \n(over 100k population)',
       x='types',
       y='suicide rates')

ggplot(test_set_updatef)+geom_histogram(aes(x=value,fill=pred_real),alpha=0.8,binwidth = 6)+
  scale_x_continuous(breaks=seq(0,140,20))+ labs(title='Predicted suicide rates vs. real suicide rates \n(over 100k population)',
                                                 x='value', y='count')+hw

## visualize the importance of these five predictors
importance(regressor)
varImpPlot(regressor) # may work on this visualization 
