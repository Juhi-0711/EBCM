require(e1071)
library(ISLR)
library(sqldf)
library(dplyr)
library(Amelia)
library(mlbench)
library(corrplot)
library(caret)
library(ellipse)
library(rlist)
library(keras)
library(textir)
library(ggplot2)




data<-read.csv(file="/Users/yashpal/Desktop/Gooru/all_data.csv",header=TRUE,sep=",")
head(data)
data<-select(data,-(user_id))
data<-select(data,status,competency_code,collection_type,everything())
data<-select(data,-(collection_type))

dim(data)
data[, 1] <- as.numeric(data[, 1]) -1
head(data)

data<- sqldf(x= "select * from data where (status=0 and total_time_spent!=0) or status=1")



#Dividing into subjects:
data_SC<- sqldf(x= "select * from data where CHARINDEX('SC',competency_code)>0") 
#print(data_SC)
dim(data_SC)


data_SS<- sqldf(x= "select * from data where CHARINDEX('SS',competency_code)>0")
summary(data_SS)
#print(data_SS)

data_ELA<- sqldf(x= "select * from data where CHARINDEX('ELA',competency_code)>0")
summary(data_ELA)
#print(data_ELA)

data_MA<- sqldf(x= "select * from data where CHARINDEX('MA',competency_code)>0") 
summary(data_MA)
#print(data_MA)


#head(data)


#Converting "Competency_code" to numeric form and normalizing it

data[, 2] <- as.numeric(factor(data[, 2]))
summary(data)
data[3:7]<-normalize(as.matrix(data[, 3:7]))

data_SC[, 2] <- as.numeric(factor(data_SC[, 2]))
#data_SC[2:7] <- normalize(as.matrix(data_SC[, 2:7]))

data_SS[, 2] <- as.numeric(factor(data_SS[, 2]))
#data_SS[2:7] <- normalize(as.matrix(data_SS[, 2:7]))

data_ELA[, 2] <- as.numeric(factor(data_ELA[, 2]))
#data_ELA[2:7] <- normalize(as.matrix(data_ELA[, 2:7]))

data_MA[, 2] <- as.numeric(factor(data_MA[, 2]))
#data_MA[2:7] <- normalize(as.matrix(data_MA[, 2:7]))


#Capturing data information about completed and in-progress

data_complete<-sqldf(x= "select * from data where status=0")
sink("/Users/yashpal/Desktop/Gooru/Summary/comp_summary.txt")
print(summary(data_complete))
sink()
data_inprogress<-sqldf(x= "select * from data where status=1")
sink("/Users/yashpal/Desktop/Gooru/Summary/ip_summary.txt")
print(summary(data_inprogress))
sink()
closeAllConnections()


#Splitting the data into test and train--->change here to change the dataset that you are working with.
set.seed(1)
n <- nrow(data)

shuffled_df <- data[sample(n), ]
train_indices <- 1:round(0.8 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.8 * n) + 1):n
test <- shuffled_df[test_indices, ]




#print (nrow(test))
#print (nrow(train))
dim(train)
dim(test)

x_train<-subset(train,select= -status)
y_train<-train$status 
head(x_train)
#svm_tune to find the parameters (cost, gamma)--->found on SC applied on everything--->done once
#svm_tune <- tune(svm, train.x=x_train, train.y=y_train, kernel="radial", ranges=list(cost=10^(-2:2), gamma=2^(-2:2)))
#print(svm_tune)

#Model Training
model <- svm(status~total_time_spent+avg_time_spent+min_time_spent+max_time_spent+num_resources+competency_code,data=train,type="C-classification",kernel="radial",cost=1,gamma=4)
summary(model)


#<------ON TRAIN----->

#Applying model on training data
preds <- predict(model,x_train)

#Plotting the data (expected vs predicted)
mydf = cbind(train, preds)
p=ggplot(train,aes(y=train$avg_time_spent, x= train$total_time_spent, color=train$status, shape=preds, size=train$num_resources/100) )
p+geom_point(alpha=0.5)

#Getting the accuracy
table(preds,y_train)
mean(preds==y_train)

#<-----ON TEST------>

x_test<-subset(test,select= -status)
y_test<-test$status 

#Applying model on test data
preds <- predict(model,x_test)

#Plotting the data (expected vs predicted)
mydf = cbind(test, preds)
p=ggplot(test,aes(y=test$avg_time_spent, x= test$total_time_spent, color=test$status, shape=preds, size=test$num_resources/100) )+ geom_point(position=position_jitter(h=0.15,w=0.15))
p+geom_point(alpha=0.5)

#Getting the accuracy
table(preds,y_test)
mean(preds==y_test)
