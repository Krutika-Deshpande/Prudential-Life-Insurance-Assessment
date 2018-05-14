#install.packages('arm')
library(readr)
library(dplyr)
library(ggplot2)
library(caTools)
library(arm)
library(MASS)
library(glmnet)

#reading the train and test data 

train_data<-read.csv('train.csv')
test_data<-read.csv('test.csv')

#checking for the number of columns and rows in train and test
dim(train_data)
#[1] 59381   128

dim(test_data)
#[1] 19765   127

#Checking for missing values 
train_missing<-apply(train_data,2,function(x)sum(is.na(x)))
na_freq<-train_missing/(nrow(train_data))

colnames(train_data[,which(na_freq>0.6)])
train_data<-train_data[-(which(na_freq>0.6))]

#test_data
test_missing<-apply(test_data,2,function(x)sum(is.na(x)))
test_na<-test_missing/(nrow(test_data))

colnames(test_data[,which(test_na>0.6)])
test_data<-test_data[-which(test_na>0.6)]

#checking for the number of columns and rows in train and test
dim(train_data)
#[1] 59381   123

dim(test_data)
#[1] 19765   122

#checking for duplicate rows for train and test
nrow(distinct(train_data))
nrow(distinct(test_data))
#no duplicate rows

#checking for constant columns in train and test data
train_const<-sapply(train_data, function(x){all(x[1L]==x)})
sum(train_const)

test_const<-sapply(test_data, function(x){length(unique(x))==1})
sum(test_const)
#no constant columns

ggplot(train_data, aes(x = train_data$Response, ))+ xlab('Response')+ geom_bar(colour = 'red') 
       + ggtitle('Plot for Response variable') + theme(plot.title = element_text(hjust = 0.5))

#splitting data into categorical, continuous and discrete for train
cols_categorical <- c(paste('Product_Info_',c(1:3,5:7),sep = ""), #paste function concatenates vectore after converting to characters
                      paste('Employment_Info_',c(2,3,5),sep=""),
                      paste('InsuredInfo_',c(1:7),sep=""),
                      paste('Insurance_History_',c(1:4,7:9),sep=""),
                      paste('Family_Hist_1'),
                      paste('Medical_History_',c(2:9,11:14,16:23,25:31,33:41),sep=""))

cols_continuous <-c('Product_Info_4','Ins_Age','Ht','Wt','BMI','Id',
                    paste('Employment_Info_',c(1,4,6),sep=""),
                    paste('Insurance_History_5'),
                    paste('Family_Hist_',c(2:4),sep=""))

cols_discrete<-c('Medical_History_1',
                 paste('Medical_Keyword_',c(1:48),sep=""))
#train
train_categorical<-train_data[,cols_categorical]
train_continuous<-train_data[,cols_continuous]
train_discrete<-train_data[,cols_discrete]

#Converting the factor to integer as the rest are integer


p2<-factor(train_categorical$Product_Info_2)
train_categorical$Product_Info_2<-as.integer(p2)


c<-apply(train_categorical, 2,function(x){length(unique(x))})
#print(c)
print(which(c>10))
train_categorical<-train_categorical[-which(c > 10)]

write.csv(train_categorical , file = 'Cat_cont_dis_split_train.csv')
  

#test
test_categorical<-test_data[,cols_categorical]
test_continuous<-test_data[,cols_continuous]
test_discrete<-test_data[,cols_discrete]

p2<-factor(test_categorical$Product_Info_2)
test_categorical$Product_Info_2<-as.integer(p2)


c<-apply(test_categorical, 2,function(x){length(unique(x))})
#print(c)
print(which(c>10))
test_categorical<-test_categorical[-which(c > 10)]

#Replace missing categorical values with median
for(i in 1:ncol(train_categorical)){
  train_categorical[is.na(train_categorical[,i]), i] <- median(train_categorical[,i], na.rm = TRUE)
}
#
for(i in 1:ncol(test_categorical)){
  test_categorical[is.na(test_categorical[,i]), i] <- median(test_categorical[,i], na.rm = TRUE)
}


#Replace missing continuous values with mean
for(i in 1:ncol(train_continuous)){
  train_continuous[is.na(train_continuous[,i]), i] <- mean(train_continuous[,i], na.rm = TRUE)
}
#
for(i in 1:ncol(test_continuous)){
  test_continuous[is.na(test_continuous[,i]), i] <- mean(test_continuous[,i], na.rm = TRUE)
}


#Replace missing discrete values with median
for(i in 1:ncol(train_discrete)){
  train_discrete[is.na(train_discrete[,i]), i] <- median(train_discrete[,i], na.rm = TRUE)
}
#
for(i in 1:ncol(test_discrete)){
  test_discrete[is.na(test_discrete[,i]), i] <- median(test_discrete[,i], na.rm = TRUE)
}

#Binding the data into dataframe

train_data = cbind(train_categorical,train_continuous,train_discrete,train_data$Response)
dim(train_data)
#write.csv(train_data , file = 'Cat_cont_dis_split_train.csv')

colnames(train_data)[which(names(train_data) == "train_data$Response")] <- "Response"

test_data = cbind(test_categorical,test_continuous,test_discrete)

#Splitting train data into training_set and test_set
set.seed(123)
split = sample.split(train_data$Response, SplitRatio = 0.75)
training_set = subset(train_data, split == TRUE)
test_set = subset(train_data, split == FALSE)

#Implementing Multiple Linear Regression
lm_model = lm(Response~., data = training_set)
summary(lm_model)



y_lm<-predict(lm_model,test_set)
y_lm<-round(y_lm)
actual_preds<-data.frame(cbind(actuals= test_set$Response, predicted = y_lm))
table(actual_preds)

min_max_accuracy <- mean (apply(actual_preds, 1, min) / apply(actual_preds, 1, max))  # => 58.42%, min_max accuracy
rmse<-sqrt(mean(lm_model$residuals))

training_set_X <- training_set[1:117]

elnet = glmnet(x = as.matrix(training_set_X),y = as.matrix(training_set$Response),alpha = 0.3, family = 'gaussian')
coefficent_elnet<-coef(elnet)
coefficent_elnet
variables<-names(coef[which(coefficent_elnet!= 0),])


