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
library(GoodmanKruskal)
library(corrplot)

## Load the training and testing data sets
data1=read.csv("train.csv")
data2=read.csv("test.csv")
head(data1)
summary(data1)

## Spearman correlation plot
cor_cot=subset(data1,select = c("Customer_Age","Dependent_count","Months_on_book","Total_Relationship_Count",
                                "Months_Inactive_12_mon", "Contacts_Count_12_mon",
                                "Credit_Limit","Total_Revolving_Bal","Avg_Open_To_Buy",
                                "Total_Amt_Chng_Q4_Q1","Total_Trans_Amt","Total_Trans_Ct",
                                "Total_Ct_Chng_Q4_Q1","Avg_Utilization_Ratio"))
corrplot(cor(cor_cot,method = "spearman"),method ="color",addCoef.col="black",tl.col="black")

## Goodman Kruskal plot
var_set=c("Attrition_Flag","Gender","Education_Level","Marital_Status","Income_Category","Card_Category")
df=subset(data1,select = var_set)
plot(GKtauDataframe(df))

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

x=model.matrix(Attrition_Flag ~.,data = data1)[,-1]
y=data1$Attrition_Flag

####################################### Lasso Model ###################################
set.seed(345)
lambda<-10^seq(-3,3,length=100)
set.seed(345)
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

####################################### Ridge model #############################
set.seed(345)
lambda<-10^seq(-3,3,length=100)
set.seed(345)
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
coef.glmnet(ridge.model$finalModel, ridge.model$bestTune$lambda)

# Make predictions on the test data
x.test <- model.matrix(Attrition_Flag ~., data2)[,-1]
probabilities <- predict(ridge.model,x.test, type = "raw")
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition_Flag)

# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition_Flag))

