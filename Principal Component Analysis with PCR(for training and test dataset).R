## Principal Component Analysis ##
set.seed(123)
df1=read.csv('Fuel Consumption.csv',header = T)
df=df1[,-c(1,2)]

str(df)
summary(df)


### Splitting dataset into training and test data
library(caTools)
split=sample.split(df$CO2EMISSIONS,0.8)
training_set=subset(df,split==T)
test_set=subset(df,split==F)

#### Multiple Linear regression model ####

mult_model=lm(CO2EMISSIONS~.,data = training_set)
mult_model
summary(mult_model)

## prediction of multiple regression model.
Multi_prediction=predict(mult_model,test_set[,-6])

## check multicollinearity 
car::vif(mult_model)

### testing dataset accurary

library(rminer)
Regression_model=mmetric(test_set$CO2EMISSIONS,Multi_prediction,
                  c("MAE","RMSE","MSE","R2"))
Regression_model


### PCA ###
pca_model=prcomp(df,center=TRUE, scale.=TRUE)
summary(pca_model)
pca_model$rotation
pcs=pca_model$x
pca_model$sdev

### PCR ###
y=df$CO2EMISSIONS
ols.data <- cbind.data.frame(y, pcs[,1:2])

#### PCs componnet trainig dataset and test dataset

PCs_training_set=subset(ols.data,split==T)
PCs_test_set=subset(ols.data,split==F)


head(ols.data)
lmodel <- lm(y ~ ., data = PCs_training_set)
lmodel
summary(lmodel)

## check multicollinearity
library(car)
vif=car::vif(lmodel)
vif

### prediction on test dataset ###
pred=predict(lmodel,PCs_test_set[,-1])
pred

### testing dataset accurary

library(rminer)
PCR_model=mmetric(PCs_test_set$y,pred,
                  c("MAE","RMSE","MSE","R2"))
PCR_model


## The estimators of coefficients that have been obtained (beta_Z), as stated in the introduction can be multiplied by matrix V to obtain beta_X.

beta_0=as.integer(lmodel$coefficients[1])
beta.Z <- as.matrix(lmodel$coefficients[2:3])

V <- as.matrix(pca_model$rotation[,c(1,2)])
V
beta.X <- V %*% beta.Z
beta.X

lmodel$coefficients


x1 <- 3.5
x2 <- 6
x3 <- 12.1
x4 <- 8.7
x5 <- 10.6

p=x1*beta.X[1]+x2*beta.X[2]+x3*beta.X[3]+x4*beta.X[4] + x5*beta.X[5]-beta_0
p
## Compare Actual, Multiple Regression prediction and PCR prediction.

Actual=test_set$CO2EMISSIONS
PCR_prediction=pred

compare=cbind.data.frame(Actual,Multi_prediction,PCR_prediction)
View(compare)
