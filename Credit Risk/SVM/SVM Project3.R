# Setting Working directory
setwd("D:\\DATASCIENCE\\DSP 22\\R programming\\PROJECTS\\Project3")
getwd()
gc()
rm(list=ls())

# Reading the given credit data
credit_train=read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.CSV",na.strings = "")
credit_test=read.csv("R_Module_Day_8.1_Credit_Risk_Test_data.CSV",na.strings = "")
credit_validate=read.csv("R_Module_Day_8.2_Credit_Risk_Validate_data.CSV",na.strings = "")

# Veiw Structure of the data set
str(credit_train)
head(credit_train)
summary(credit_train)
# So the data contains NA

#Imputing missing values in training data

table(credit_train$Gender)
credit_train[which(is.na(credit_train$Gender)),"Gender"] = "Male"
table(credit_train$Married)
credit_train[which(is.na(credit_train$Married)),"Married"] = "Yes"
table(credit_train$Dependents)
credit_train[which(is.na(credit_train$Dependents)),"Dependents"] = "0"
table(credit_train$Self_Employed)
credit_train[which(is.na(credit_train$Self_Employed)),"Self_Employed"] = "No"
credit_train[which(is.na(credit_train$LoanAmount)),"LoanAmount"] =
  median(credit_train$LoanAmount,na.rm = T)
credit_train[which(is.na(credit_train$Loan_Amount_Term)),"Loan_Amount_Term"] =
  median(credit_train$Loan_Amount_Term,na.rm = T)
table(credit_train$Credit_History)
credit_train[which(is.na(credit_train$Credit_History)),"Credit_History"] = 1
summary(credit_train)

#Imputing missing values in validation data

str(credit_validate)
head(credit_validate)
summary(credit_validate)
table(credit_validate$Gender)
credit_validate[which(is.na(credit_validate$Gender)),"Gender"] = "Male"
table(credit_validate$Dependents)
credit_validate[which(is.na(credit_validate$Dependents)),"Dependents"] = "0"
table(credit_validate$Self_Employed)
credit_validate[which(is.na(credit_validate$Self_Employed)),"Self_Employed"] = "No"
credit_validate[which(is.na(credit_validate$LoanAmount)),"LoanAmount"] =
  median(credit_validate$LoanAmount,na.rm = T)
credit_validate[which(is.na(credit_validate$Loan_Amount_Term)),"Loan_Amount_Term"] =
  median(credit_validate$Loan_Amount_Term,na.rm = T)
table(credit_validate$Credit_History)
credit_validate[which(is.na(credit_validate$Credit_History)),"Credit_History"] = 1
summary(credit_validate)

#Imputing missing values in Testing data

str(credit_test)
head(credit_test)
summary(credit_test)
table(credit_test$Gender)
credit_test[which(is.na(credit_test$Gender)),"Gender"] = "Male"
table(credit_test$Dependents)
credit_test[which(is.na(credit_test$Dependents)),"Dependents"] = "0"
table(credit_test$Self_Employed)
credit_test[which(is.na(credit_test$Self_Employed)),"Self_Employed"] = "No"
credit_test[which(is.na(credit_test$LoanAmount)),"LoanAmount"] =
  median(credit_test$LoanAmount,na.rm = T)
credit_test[which(is.na(credit_test$Loan_Amount_Term)),"Loan_Amount_Term"] =
  median(credit_test$Loan_Amount_Term,na.rm = T)
table(credit_test$Credit_History)
credit_test[which(is.na(credit_test$Credit_History)),"Credit_History"] = 1
summary(credit_test)

# Exploratory Data Analysis

table(credit_train$Loan_Status)
plot(credit_train$Gender,credit_train$Loan_Status, xlab = "Gender", ylab = "Loan Status", col = c("red","green"), main = "Gender impact on Loan Status")
plot(credit_train$Married,credit_train$Loan_Status, xlab = "Marital Status", ylab = "Loan Status", col = c("red","green"), main = "Marital Status impact on Loan Status")
plot(credit_train$Dependents,credit_train$Loan_Status, xlab = "Number of Dependents", ylab = "Loan Status", col = c("red","green"), main = "No. of Dependents impact on Loan Status")
plot(credit_train$Education,credit_train$Loan_Status, xlab = "Education Status", ylab = "Loan Status", col = c("red","green"), main = "Education impact on Loan Status")
plot(credit_train$Self_Employed,credit_train$Loan_Status, xlab = "Self Employment Status", ylab = "Loan Status", col = c("red","green"), main = "Self-Employment impact on Loan Status")
boxplot(credit_train$ApplicantIncome~credit_train$Loan_Status,xlab = "Loan Status",ylab = "Applicant Income", col = c("red","green"), main = "Applicant's Income impact on Loan Status")
boxplot(credit_train$CoapplicantIncome~credit_train$Loan_Status,xlab = "Loan Status",ylab = "Co-Applicant Income", col = c("red","green"), main = "Co-Applicant's Income impact on Loan Status")
boxplot(credit_train$LoanAmount~credit_train$Loan_Status,xlab = "Loan Status",ylab = "Loan Amount", col = c("red","green"), main = "Loan Amount impact on Loan Status")
plot(as.factor(credit_train$Loan_Amount_Term),credit_train$Loan_Status,xlab = "Loan Amount Term",ylab = "Loan Status", col = c("red","green"), main = "Loan Amount Term impact on Loan Status")
plot(as.factor(credit_train$Credit_History),credit_train$Loan_Status,xlab = "Credit History",ylab = "Loan Status", col = c("red","green"), main = "Credit History impact on Loan Status")
plot(credit_train$Property_Area,credit_train$Loan_Status,xlab = "Property Area",ylab = "Loan Status", col = c("red","green"), main = "Property Area impact on Loan Status")

str(credit_train)
install.packages("e1071")
library(e1071)
model_svm_1= svm(Loan_Status~.-Loan_ID,data= credit_train, kernel= "linear")
summary(model_svm_1)

pred= predict(model_svm_1,newdata = credit_validate)
pred
table(credit_validate$outcome,pred)
Accuracy= (58+289)/(58+289+19+1)
Accuracy

install.packages("caret")

#Data Transformation for Training data

str(credit_train)
head(credit_train)
credit_train$Gender=as.numeric(ifelse(credit_train$Gender== 'Male',1,0))
credit_train$Married=as.numeric(ifelse(credit_train$Married== 'Yes',1,0))
credit_train$Education=as.numeric(ifelse(credit_train$Education== 'Graduate',1,0))
credit_train$Self_Employed=as.numeric(ifelse(credit_train$Self_Employed== 'Yes',1,0))
credit_train$Property_Area_Rural= as.numeric(ifelse(credit_train$Property_Area== 'Rural',1,0))
credit_train$Property_Area_Urban= as.numeric(ifelse(credit_train$Property_Area== 'Urban',1,0))
#credit_train$Loan_Status=as.numeric(ifelse(credit_train$Loan_Status== 'Y',1,0))
credit_train$Dependents= as.numeric(ifelse(as.character(credit_train$Dependents)=='3+','3',as.character(credit_train$Dependents)))
credit_train$Credit_History = as.numeric(credit_train$Credit_History)

# Removing the columns which are not needed 
credit_train$Loan_ID=NULL
credit_train$Property_Area=NULL
str(credit_train)

#Data Transformation for Validation data

str(credit_validate)
head(credit_validate)
credit_validate$Gender=as.numeric(ifelse(credit_validate$Gender== 'Male',1,0))
credit_validate$Married=as.numeric(ifelse(credit_validate$Married== 'Yes',1,0))
credit_validate$Education=as.numeric(ifelse(credit_validate$Education== 'Graduate',1,0))
credit_validate$Self_Employed=as.numeric(ifelse(credit_validate$Self_Employed== 'Yes',1,0))
credit_validate$Property_Area_Rural= as.numeric(ifelse(credit_validate$Property_Area== 'Rural',1,0))
credit_validate$Property_Area_Urban= as.numeric(ifelse(credit_validate$Property_Area== 'Urban',1,0))
#credit_validate$outcome=as.numeric(ifelse(credit_validate$outcome== 'Y',1,0))
credit_validate$Dependents= as.numeric(ifelse(as.character(credit_validate$Dependents)=='3+','3',as.character(credit_validate$Dependents)))
credit_validate$Credit_History = as.numeric(credit_validate$Credit_History)

# Removing the columns which are not needed 
credit_validate$Loan_ID=NULL
credit_validate$Property_Area=NULL
str(credit_validate)

#Data Transformation for Test data

str(credit_test)
head(credit_test)
credit_test$Gender=as.numeric(ifelse(credit_test$Gender== 'Male',1,0))
credit_test$Married=as.numeric(ifelse(credit_test$Married== 'Yes',1,0))
credit_test$Education=as.numeric(ifelse(credit_test$Education== 'Graduate',1,0))
credit_test$Self_Employed=as.numeric(ifelse(credit_test$Self_Employed== 'Yes',1,0))
credit_test$Property_Area_Rural= as.numeric(ifelse(credit_test$Property_Area== 'Rural',1,0))
credit_test$Property_Area_Urban= as.numeric(ifelse(credit_test$Property_Area== 'Urban',1,0))
credit_test$Dependents= as.numeric(ifelse(as.character(credit_test$Dependents)=='3+','3',as.character(credit_test$Dependents)))
credit_test$Credit_History = as.numeric(credit_test$Credit_History)

# Removing the columns which are not needed

credit_test$Property_Area=NULL
str(credit_test)

# Fitting SVM

set.seed(8)
library(e1071)
model_svm= svm (Loan_Status~.,data=credit_train,kernel="linear",cost=0.5,gamma=0.05)
summary(model_svm)
str(credit_validate)
predic= predict(model_svm,newdata= credit_validate)
predic

#Confusion Matrix

table(credit_validate$outcome,predic)
Accuracy= (58+289)/(58+289+1+19)
Accuracy

#0ther SVM Method

library(caret)
str(credit_train)
model_svm_2= train(credit_train[,c(1:10,12,13)],credit_train$Loan_Status,method = "svmLinear")
model_svm_2
pred= predict(model_svm_2,newdata = credit_validate[,c(1:10,12,13)])
confusionMatrix(credit_validate$outcome,pred)

#Save predictions for the testing dataset

str(credit_test)
predic= predict(model_svm_2,newdata=credit_test[,c(2:13)])
predic
str(predic)
results= data.frame(Loan_ID=credit_test$Loan_ID,predicted_Loan_Status=predic)
head(results)
table(results$predicted_Loan_Status)
write.csv(results,"Predictions_Loan_Status.CSV",row.names = F)
