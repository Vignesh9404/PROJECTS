#Setting the working directory
setwd("D:\\DATASCIENCE\\DSP 22\\R programming\\PROJECTS\\Housing Pricing")
getwd()
rm(list = ls())
gc()
set.seed(123)

#Reading the directory
train=read.csv("train.csv",header=T,stringsAsFactors = F)
head(train,10)
tail(train,10)
str(train)
View(train)

#Checking Summary of the Data

summary(train)

#Checking the na variables
table(train$LotFrontage)
train[which(is.na(train$LotFrontage)),"LotFrontage"]=mean(train$LotFrontage,na.rm = T)

table(train$MasVnrArea)
train[which(is.na(train$MasVnrArea)),"MasVnrArea"]=mean(train$MasVnrArea,na.rm = T)

table(train$GarageYrBlt)
train$GarageYrBlt[is.na(train$GarageYrBlt)] = train$YearBuilt[is.na(train$GarageYrBlt)]
train$GarageYrBlt


test=read.csv("test.csv",header=T,stringsAsFactors = F)
summary(test)


table(test$LotFrontage)
test[which(is.na(test$LotFrontage)),"LotFrontage"]=median(test$LotFrontage,na.rm = T)
test$LotFrontage

test[which(is.na(test$MasVnrArea)),"MasVnrArea"]=median(test$MasVnrArea,na.rm = T)
test$MasVnrArea

test[which(is.na(test$BsmtFinSF1)),"BsmtFinSF1"]=median(test$BsmtFinSF1,na.rm = T)
test$BsmtFinSF1

test[which(is.na(test$BsmtFinSF2)),"BsmtFinSF2"]=median(test$BsmtFinSF2,na.rm = T)
test$BsmtFinSF2

summary(test)

test[which(is.na(test$BsmtUnfSF)),"BsmtUnfSF"]=median(test$BsmtUnfSF,na.rm = T)
test$BsmtUnfSF

test[which(is.na(test$TotalBsmtSF)),"TotalBsmtSF"]=median(test$TotalBsmtSF,na.rm = T)
test$TotalBsmtSF

test[which(is.na(test$BsmtFullBath)),"BsmtFullBath"]=median(test$BsmtFullBath,na.rm = T)
test$BsmtFullBath

test[which(is.na(test$BsmtHalfBath)),"BsmtHalfBath"]=median(test$BsmtHalfBath,na.rm = T)
test$BsmtHalfBath 

test$GarageYrBlt[is.na(test$GarageYrBlt)] = test$YearBuilt[is.na(test$GarageYrBlt)]
test$GarageYrBlt

summary(test)

test[which(is.na(test$GarageCars)),"GarageCars"]="2"
test[which(is.na(test$GarageArea)),"GarageArea"]=mean(test$GarageArea,na.rm = T)

test$GarageArea
test$GarageCars

summary(test)

str(train)


#Converting the char variables to factors
train$MSZoning=factor(train$MSZoning)
train$MSZoning=as.numeric(train$MSZoning)
levels(train$MSZoning)
str(train$MSZoning)

train$Street=factor(train$Street)
train$Street=as.numeric(train$Street)

train$Alley=factor(train$Alley,exclude=NULL)
train$Alley=as.numeric(train$Alley)

train$LotShape=factor(train$LotShape,exclude=NULL)
train$LotShape=as.numeric(train$LotShape)


train$LandContour=factor(train$LandContour,exclude=NULL)
train$LandContour=as.numeric(train$LandContour)

train$Utilities=factor(train$Utilities,exclude=NULL)
train$Utilities=as.numeric(train$Utilities)

train$LotConfig=factor(train$LotConfig,exclude=NULL)
train$LotConfig=as.numeric(train$LotConfig)

train$LandSlope=factor(train$LandSlope,exclude=NULL)
train$LandSlope=as.numeric(train$LandSlope)

train$Neighborhood=factor(train$Neighborhood,exclude=NULL)
train$Neighborhood=as.numeric(train$Neighborhood)

train$Condition1=factor(train$Condition1,exclude=NULL)
train$Condition1=as.numeric(train$Condition1)

train$Condition2=factor(train$Condition2,exclude=NULL)
train$Condition2=as.numeric(train$Condition2)

train$BldgType=factor(train$BldgType,exclude=NULL)
train$BldgType=as.numeric(train$BldgType)

train$HouseStyle=factor(train$HouseStyle,exclude=NULL)
train$HouseStyle=as.numeric(train$HouseStyle)

train$RoofStyle=factor(train$RoofStyle,exclude=NULL)
train$RoofStyle=as.numeric(train$RoofStyle)

train$RoofMatl=factor(train$RoofMatl,exclude=NULL)
train$RoofMatl=as.numeric(train$RoofMatl)

train$Exterior1st=factor(train$Exterior1st,exclude=NULL)
train$Exterior1st=as.numeric(train$Exterior1st)

train$Exterior2nd=factor(train$Exterior2nd,exclude=NULL)
train$Exterior2nd=as.numeric(train$Exterior2nd)

train$MasVnrType=factor(train$MasVnrType,exclude=NULL)
train$MasVnrType=as.numeric(train$MasVnrType)

train$ExterQual=factor(train$ExterQual,exclude=NULL)
train$ExterQual=as.numeric(train$ExterQual)

train$ExterCond=factor(train$ExterCond,exclude=NULL)
train$ExterCond=as.numeric(train$ExterCond)

train$Foundation=factor(train$Foundation,exclude=NULL)
train$Foundation=as.numeric(train$Foundation)

train$BsmtQual=factor(train$BsmtQual,exclude=NULL)
train$BsmtQual=as.numeric(train$BsmtQual)

train$BsmtCond=factor(train$BsmtCond,exclude=NULL)
train$BsmtCond=as.numeric(train$BsmtCond)

train$BsmtExposure=factor(train$BsmtExposure,exclude=NULL)
train$BsmtExposure=as.numeric(train$BsmtExposure)

train$BsmtFinType1=factor(train$BsmtFinType1,exclude=NULL)
train$BsmtFinType1=as.numeric(train$BsmtFinType1)

train$BsmtFinType2=factor(train$BsmtFinType2,exclude=NULL)
train$BsmtFinType2=as.numeric(train$BsmtFinType2)

train$Heating=factor(train$Heating,exclude=NULL)
train$Heating=as.numeric(train$Heating)

train$HeatingQC=factor(train$HeatingQC,exclude=NULL)
train$HeatingQC=as.numeric(train$HeatingQC)

train$CentralAir=factor(train$CentralAir,exclude=NULL)
train$CentralAir=as.numeric(train$CentralAir)

train$Electrical=factor(train$Electrical,exclude=NULL)
train$Electrical=as.numeric(train$Electrical)

train$KitchenQual=factor(train$KitchenQual,exclude=NULL)
train$KitchenQual=as.numeric(train$KitchenQual)

train$Functional=factor(train$Functional,exclude=NULL)
train$Functional=as.numeric(train$Functional)

train$FireplaceQu=factor(train$FireplaceQu,exclude=NULL)
train$FireplaceQu=as.numeric(train$FireplaceQu)

train$GarageType=factor(train$GarageType,exclude=NULL)
train$GarageType=as.numeric(train$GarageType)

train$GarageFinish=factor(train$GarageFinish,exclude=NULL)
train$GarageFinish=as.numeric(train$GarageFinish)

train$GarageQual=factor(train$GarageQual,exclude=NULL)
train$GarageQual=as.numeric(train$GarageQual)

train$GarageCond=factor(train$GarageCond,exclude=NULL)
train$GarageCond=as.numeric(train$GarageCond)

train$PavedDrive=factor(train$PavedDrive,exclude=NULL)
train$PavedDrive=as.numeric(train$PavedDrive)

train$PoolQC=factor(train$PoolQC,exclude=NULL)
train$PoolQC=as.numeric(train$PoolQC)

train$Fence=factor(train$Fence,exclude=NULL)
train$Fence=as.numeric(train$Fence)

train$MiscFeature=factor(train$MiscFeature,exclude=NULL)
train$MiscFeature=as.numeric(train$MiscFeature)

train$SaleType=factor(train$SaleType,exclude=NULL)
train$SaleType=as.numeric(train$SaleType)

train$SaleCondition=factor(train$SaleCondition,exclude=NULL)
train$SaleCondition=as.numeric(train$SaleCondition)

str(train)

#Converting the char variables to factors
summary(test)

test$MSZoning=factor(test$MSZoning)
test$MSZoning=as.numeric(test$MSZoning)
levels(test$MSZoning)
str(test$MSZoning)

test$Street=factor(test$Street)
test$Street=as.numeric(test$Street)

test$Alley=factor(test$Alley,exclude=NULL)
test$Alley=as.numeric(test$Alley)

test$LotShape=factor(test$LotShape,exclude=NULL)
test$LotShape=as.numeric(test$LotShape)


test$LandContour=factor(test$LandContour,exclude=NULL)
test$LandContour=as.numeric(test$LandContour)

test$Utilities=factor(test$Utilities,exclude=NULL)
test$Utilities=as.numeric(test$Utilities)

test$LotConfig=factor(test$LotConfig,exclude=NULL)
test$LotConfig=as.numeric(test$LotConfig)

test$LandSlope=factor(test$LandSlope,exclude=NULL)
test$LandSlope=as.numeric(test$LandSlope)

test$Neighborhood=factor(test$Neighborhood,exclude=NULL)
test$Neighborhood=as.numeric(test$Neighborhood)

test$Condition1=factor(test$Condition1,exclude=NULL)
test$Condition1=as.numeric(test$Condition1)

test$Condition2=factor(test$Condition2,exclude=NULL)
test$Condition2=as.numeric(test$Condition2)

test$BldgType=factor(test$BldgType,exclude=NULL)
test$BldgType=as.numeric(test$BldgType)

test$HouseStyle=factor(test$HouseStyle,exclude=NULL)
test$HouseStyle=as.numeric(test$HouseStyle)

test$RoofStyle=factor(test$RoofStyle,exclude=NULL)
test$RoofStyle=as.numeric(test$RoofStyle)

test$RoofMatl=factor(test$RoofMatl,exclude=NULL)
test$RoofMatl=as.numeric(test$RoofMatl)

test$Exterior1st=factor(test$Exterior1st,exclude=NULL)
test$Exterior1st=as.numeric(test$Exterior1st)

test$Exterior2nd=factor(test$Exterior2nd,exclude=NULL)
test$Exterior2nd=as.numeric(test$Exterior2nd)

test$MasVnrType=factor(test$MasVnrType,exclude=NULL)
test$MasVnrType=as.numeric(test$MasVnrType)

test$ExterQual=factor(test$ExterQual,exclude=NULL)
test$ExterQual=as.numeric(test$ExterQual)

test$ExterCond=factor(test$ExterCond,exclude=NULL)
test$ExterCond=as.numeric(test$ExterCond)

test$Foundation=factor(test$Foundation,exclude=NULL)
test$Foundation=as.numeric(test$Foundation)

test$BsmtQual=factor(test$BsmtQual,exclude=NULL)
test$BsmtQual=as.numeric(test$BsmtQual)

test$BsmtCond=factor(test$BsmtCond,exclude=NULL)
test$BsmtCond=as.numeric(test$BsmtCond)

test$BsmtExposure=factor(test$BsmtExposure,exclude=NULL)
test$BsmtExposure=as.numeric(test$BsmtExposure)

test$BsmtFinType1=factor(test$BsmtFinType1,exclude=NULL)
test$BsmtFinType1=as.numeric(test$BsmtFinType1)

test$BsmtFinType2=factor(test$BsmtFinType2,exclude=NULL)
test$BsmtFinType2=as.numeric(test$BsmtFinType2)

test$Heating=factor(test$Heating,exclude=NULL)
test$Heating=as.numeric(test$Heating)

test$HeatingQC=factor(test$HeatingQC,exclude=NULL)
test$HeatingQC=as.numeric(test$HeatingQC)

test$CentralAir=factor(test$CentralAir,exclude=NULL)
test$CentralAir=as.numeric(test$CentralAir)

test$Electrical=factor(test$Electrical,exclude=NULL)
test$Electrical=as.numeric(test$Electrical)

test$KitchenQual=factor(test$KitchenQual,exclude=NULL)
test$KitchenQual=as.numeric(test$KitchenQual)

test$Functional=factor(test$Functional,exclude=NULL)
test$Functional=as.numeric(test$Functional)

test$FireplaceQu=factor(test$FireplaceQu,exclude=NULL)
test$FireplaceQu=as.numeric(test$FireplaceQu)

test$GarageType=factor(test$GarageType,exclude=NULL)
test$GarageType=as.numeric(test$GarageType)

test$GarageFinish=factor(test$GarageFinish,exclude=NULL)
test$GarageFinish=as.numeric(test$GarageFinish)

test$GarageQual=factor(test$GarageQual,exclude=NULL)
test$GarageQual=as.numeric(test$GarageQual)

test$GarageCond=factor(test$GarageCond,exclude=NULL)
test$GarageCond=as.numeric(test$GarageCond)

test$PavedDrive=factor(test$PavedDrive,exclude=NULL)
test$PavedDrive=as.numeric(test$PavedDrive)

test$PoolQC=factor(test$PoolQC,exclude=NULL)
test$PoolQC=as.numeric(test$PoolQC)

test$Fence=factor(test$Fence,exclude=NULL)
test$Fence=as.numeric(test$Fence)

test$MiscFeature=factor(test$MiscFeature,exclude=NULL)
test$MiscFeature=as.numeric(test$MiscFeature)

test$SaleType=factor(test$SaleType,exclude=NULL)
test$SaleType=as.numeric(test$SaleType)

test$SaleCondition=factor(test$SaleCondition,exclude=NULL)
test$SaleCondition=as.numeric(test$SaleCondition)


str(test)
str(train)

test$GarageCars=as.numeric(test$GarageCars)
str(test)


#Correlation
cr=cor(train)
cr
library(corrplot)
corrplot(cr,type="lower",method = "circle")
corrplot(cr,type="lower",method = "number")

#Fitting model
#Basic Model
model=lm(SalePrice~.-Id,data =train)
summary(model)
str(train)

#Adding significant predictors from model
model=lm(SalePrice~.-Id,data =train)
model1=lm(SalePrice~MSSubClass+LotFrontage+LotArea+Street+Alley+LandContour+Neighborhood+Condition2+BldgType+OverallQual+OverallCond+YearBuilt+RoofStyle+RoofMatl+Exterior1st+MasVnrType+MasVnrArea+ExterQual+BsmtQual+BsmtCond+BsmtExposure+X1stFlrSF+X2ndFlrSF+BsmtFullBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+GarageCars+WoodDeckSF+ScreenPorch+PoolArea+PoolQC+SaleCondition,data=train)
summary(model1)

#Adding significant predictors from model1
model2=lm(SalePrice~MSSubClass+LotFrontage+LotArea+Street+Condition2+OverallQual+OverallCond+YearBuilt+RoofMatl+Exterior1st+MasVnrType+MasVnrArea+ExterQual+BsmtQual+BsmtCond+BsmtExposure+X1stFlrSF+X2ndFlrSF+BsmtFullBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+GarageCars+WoodDeckSF+ScreenPorch+PoolArea+PoolQC+SaleCondition,data=train)
summary(model2)

#dealing with Multi collineraity
AIC(model)
AIC(model1)
AIC(model2)

#Residual Diagonsitics
plot(model1,1) #checking the Linarity assumption
plot(model1,2) #checking the normality assumption
plot(model1,3) #checking the homoskedascity assumption
plot(model1,4) #chekcing the Cook's distance
plot(model1,5) #checking influential points

#Examine Univarite distributions
hist(resid(model1),freq=F)
boxplot(resid(model1))

#sum of residuals is always zero
sum(resid(model1))


#No auto correlation btwn residuals
acf(resid(model1))

#Durbin-watson test
install.packages("lmtest")
library(lmtest)
dwtest(model1)

#All independent variables are uncorrelated with the error term
cor(resid(model1),train[,c(2:81)])

#Mean of errors is zero
mean(resid(model1))

#Fitting the multiple linear regression on the LGD dataset

model3=lm(log(SalePrice)~MSSubClass+LotFrontage+LotArea+Street+Alley+LandContour+Neighborhood+Condition2+BldgType+OverallQual+OverallCond+YearBuilt+RoofStyle+RoofMatl+Exterior1st+MasVnrType+MasVnrArea+ExterQual+BsmtQual+BsmtCond+BsmtExposure+X1stFlrSF+X2ndFlrSF+BsmtFullBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+GarageCars+WoodDeckSF+ScreenPorch+PoolArea+PoolQC+SaleCondition,data=train)
summary(model3)

model3=lm(log(SalePrice)~MSSubClass+LotArea+Street+Condition2+OverallQual+OverallCond+YearBuilt+BsmtQual+BsmtExposure+X1stFlrSF+X2ndFlrSF+BsmtFullBath+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+GarageCars+WoodDeckSF+ScreenPorch+PoolArea+PoolQC+SaleCondition,data=train)
summary(model3)


#Residual Diagonsitics

plot(model3,1) #checking the Linarity assumption
plot(model3,2) #checking the normality assumption
plot(model3,3) #checking the homoskedascity assumption
plot(model3,4) #chekcing the Cook's distance
plot(model3,5) #checking influential points

#Examine Univarite distributions

hist(resid(model3),freq=F)
boxplot(resid(model3))

#sum of residuals is always zero

sum(resid(model3))

#No auto correlation btwn residuals

acf(resid(model3))

#Durbin-watson test

library(lmtest)
dwtest(model3)

#All independent variables are uncorrelated with the error term

cor(resid(model3),train[,c(2:81)])

#Mean of errors is zero

mean(resid(model3))
mean(resid(model1))

#Predictions
pred_test=predict(model1,newdata =test)
pred_test

pred_test1=predict(model3,newdata =test)

table(pred_test1)

test$SalePrice=exp(pred_test1)
table(test$SalePrice)

AIC(model1)
AIC(model3)


Submission = data.frame(Id = test$Id, SalePrice= test$SalePrice)
write.csv(Submission,"Sale price for testing data.csv",row.names=F)
