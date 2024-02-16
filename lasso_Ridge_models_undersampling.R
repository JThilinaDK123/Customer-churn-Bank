library(knitr)
library(recipes)  
library(glmnet)   
library(caret)    
library(vip) 
library(pls)
library(leaps)
library(pdp)
library(ggplot2)
library(ROSE)

## Load the dataset
data1=read.csv("train.csv")
data2=read.csv("test.csv")


data1$Attrition_Flag<-factor(data1$Attrition_Flag,
                             levels = c("Existing","Attrited"),
                             labels = c(0,1))

data2$Attrition_Flag<-factor(data2$Attrition_Flag,
                             levels = c("Existing","Attrited"),
                             labels = c(0,1))

cols=c("Gender","Education_Level","Marital_Status","Income_Category","Card_Category")
data1[cols] <- lapply(data1[cols], factor)
data2[cols] <- lapply(data2[cols], factor)
head(data1)
summary(data1$Attrition_Flag)

#undersampling

data_balanced_under <- ovun.sample(Attrition_Flag ~ ., data = data1, method = "under",N =2604,seed = 1)$data
table(data_balanced_under$Attrition_Flag)
x=model.matrix(Attrition_Flag~.,data_balanced_under)[,-1]
dim(x)
y=data_balanced_under$Attrition_Flag
y

####################### lasso ##################
set.seed(345)
lambda<-10^seq(-3,3,length=100)
lasso.model<-train(
  x = x,
  y = y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10,
  family="binomial",
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
  
)

# Display regression coefficients
coef(lasso.model$finalModel, lasso.model$bestTune$lambda)
coef.glmnet(lasso.model$finalModel, lasso.model$bestTune$lambda)

# Make predictions on the test data
x.test <- model.matrix(Attrition_Flag ~., data2)[,-1]
probabilities <- predict(lasso.model,x.test, type = "raw")
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition_Flag)

# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition_Flag))

#plots
#vip
vip(lasso.model, num_features = 45, geom = "point")

####################### Ridge #####################

set.seed(345)
lambda<-10^seq(-3,3,length=100)
ridge.model<-train(
  x = x,
  y = y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10,
  family="binomial",
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  
)

# Display regression coefficients
coef(ridge.model$finalModel, ridge.model$bestTune$lambda)
coef.glmnet(ridge.model$finalModel,ridge.model$bestTune$lambda)

# Make predictions on the test data
x.test <- model.matrix(Attrition_Flag ~., data2)[,-1]
dim(x.test)
probabilities <- predict(ridge.model,x.test, type = "raw")
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition_Flag)

# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition_Flag))
