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


#################################### No sampling ##################################

library(randomForest)
set.seed(345)
model1=randomForest(formula=Attrition_Flag~.,data = data1)
predictions=model1 %>% predict(data2,type = "class")
confusionMatrix(predictions,data2$Attrition_Flag)

############################### over-sampling #####################################

data3<- ovun.sample(Attrition_Flag ~ ., data = data1, method = "over",N =13600)$data
table(data3$Attrition_Flag)
set.seed(345)
model2=randomForest(formula=Attrition_Flag~.,data = data3)
predictions=model2 %>% predict(data2,type = "class")
confusionMatrix(predictions,data2$Attrition_Flag)

############################### under-sampling #####################################

data4 <- ovun.sample(Attrition_Flag ~ ., data = data1, method = "under",N =2604,seed = 1)$data
table(data3$Attrition_Flag)
set.seed(345)
model3=randomForest(formula=Attrition_Flag~.,data = data4)
predictions=model3 %>% predict(data2,type = "class")
confusionMatrix(predictions,data2$Attrition_Flag)
