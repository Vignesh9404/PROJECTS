#Setting working directory
setwd("D:\\DATASCIENCE\\DSP 22\\R programming\\PROJECTS\\Project4")

library(dplyr)
set.seed(123)
gc()
rm(list=ls())

# Read the datasets
nw_train<-read.csv("Network_Intrusion_Train_data.csv")
nw_test<-read.csv("Network_Intrusion_Test_data.csv")
nw_valid<-read.csv("Network_Intrusion_Validate_data.csv")

#Check the structure of dataset
str(nw_train)
str(nw_test)
str(nw_valid)

# Check the summary of datasets
summary(nw_train)
summary(nw_test)
summary(nw_valid)

# use the below function  on the training dataset
dim(nw_train)
head(nw_train)
str(nw_train)
summary(nw_train)

#Plot the relationship between some of the discrete variables and the output variable

plot(nw_train$protocol_type, nw_train$class, main="Protocal Type vs Class",xlab="Protocol type", ylab="Class", col=c("red","lightgreen"))
plot(nw_train$service, nw_train$class, main="Service vs Class",xlab="Service", ylab="Class", col=c("blue","grey"))
plot(nw_train$flag, nw_train$class, main="Flag vs Class",xlab="Flag", ylab="Class", col=c("brown","lightblue"))

plot(as.factor(nw_train$logged_in),nw_train$class, xlab="logged In", ylab="Class", col=c("pink","lightyellow"))
plot(as.factor(nw_train$is_host_login),nw_train$class, xlab="Is Host Login", ylab="Class", col=c("lightblue","yellow"))

#Create the cart model using rpart
library(rpart)

cart_mod<-rpart(class~.,data=nw_train, method="class")
summary(cart_mod)

# Plotting Decision Tree
plot(cart_mod,margin = 0.01)
text(cart_mod, use.n = T, pretty = T, cex=1)

# Plotting Decision Tree
  plot(cart_mod,margin = 0.08)
text(cart_mod, use.n = T, pretty = T, cex=0.59)


#Lets do the predictions on the validation dataset using cart model

#Resolve error
levels(nw_valid$service)=levels(nw_train$service)
#agaian perform the prediction  on the validation dataset using the model on decision tree
pred_on_valid<-predict(cart_mod, newdata = nw_valid, type = "class")

pred_on_valid

table(nw_valid$class, pred_on_valid)

# calculate the accuracy ratio

accuracy_ratio_of_cart_mod<-(7672+9380)/(7672+331+5161+9380)
accuracy_ratio_of_cart_mod

# Perform the prediction for test dataset
levels(nw_test$service)=levels(nw_train$service)
pred_on_nw_test<-predict(cart_mod,newdata = nw_test, typr="class")
pred_on_nw_test

table(pred_on_nw_test)

# Alternate mehtod of plotting
library(rpart.plot)
library(RColorBrewer)
prp(cart_mod)

# find out the cp parameter  corresponding to the least cross validation error

printcp(cart_mod)

#Prune the tree using cp parameter corresponding to the least cross validation error
cart_mod_1<-prune(cart_mod, cp=0.01)
prp(cart_mod_1)

# prediction on validation data using pruned tree
pred_on_valid_1<-predict(cart_mod_1, newdata = nw_valid, type = "class")
table(pred_on_valid_1)
table(nw_valid$class,pred_on_valid_1)

accuracy_model_1<-(7672+9380)/(7672+331+5161+9380)
accuracy_model_1

# There is no change in model accuracy by default model using the least cross validation error and the corresponding cp parameters

# create a new model by using a different cp parameter

cart_mod_2<-prune(cart_mod, cp=0.045559)

prp(cart_mod_2)

# Prediction on validation dataset using model2
pred_on_valid_2<-predict(cart_mod_2,newdata = nw_valid, type = "class")
table(pred_on_valid_2)

table(nw_valid$class, pred_on_valid_2)

accuracy_model_2<-(9468+9195)/(9768+516+3365+9195)
accuracy_model_2

# Prediction on test dataset using model2
pred_on_nw_test_1<-predict(cart_mod_2,newdata = nw_test, type = "class")
pred_on_nw_test_1
table(pred_on_nw_test_1)

results<-data.frame(Duration=nw_test$duration, Protocal_type=nw_test$protocol_type, Service=nw_test$service,
                    flag=nw_test$flag, Predicted_class=pred_on_nw_test_1)

head(results,10)

write.csv(results, "Network_Anamoly_Detection.csv")

# Decision Tree model2 accuracy predicted as 81.6%

#Random Forest (Ensemble learning)

install.packages("randomForest")
library(randomForest)

levels(nw_test$service)=levels(nw_train$service)
ran_forest_mod<-randomForest(class~.,data = nw_train, method="class")

#Random forest cannot handle more than 53 categorical predictors

str(nw_train)
nw_train$service<-as.numeric(nw_train$service)
nw_test$service<-as.numeric(nw_test$service)
nw_valid$service<-as.numeric(nw_valid$service)

ran_forest_mod<-randomForest(class~.,data = nw_train, method="class")

# Apply Random forest model on validation dataset


pred_rnd_forest<-predict(ran_forest_mod,newdata=nw_valid,type="class")

# Create the confusion matrix
table(nw_valid$class, pred_rnd_forest)


ran_forest_mod_accuracy<-(8153+9441)/(8153+270+4680+9441)

ran_forest_mod_accuracy

# Identification of important variables

varImpPlot(ran_forest_mod)

# Do the prediction of Important variables
pred_rnd_forest_test_data<-predict(ran_forest_mod, newdata = nw_test,type = "class")

table(pred_rnd_forest_test_data)

# Store the results in new dataframe called Network intrusion Random Forest

Network_Intution_RF<-data.frame(Duration=nw_test$duration,Protocol_Type=nw_test$protocol_type,
                                Service=as.factor(nw_test$service),Flag=nw_test$flag,
                                Predicted_class=pred_rnd_forest_test_data)
head(Network_Intution_RF)

write.csv(Network_Intution_RF, "Network_Anamoly_Detection_Random_forest.csv",row.names = F)
