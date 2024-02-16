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


############################### Under-sampling ##################################

data3 <- ovun.sample(Attrition_Flag ~ ., data = data1, method = "under",N =2604,seed = 1)$data
table(data3$Attrition_Flag)

set.seed(345)
model1=train(
  Attrition_Flag~.,data = data3,method = "knn",
  trControl = trainControl("cv",number = 10),
  tuneLength = 10
)

# Make predictions on the test data
probabilities <- model1 %>% predict(data2)
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition_Flag)

# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition_Flag))


############################### Over-sampling ##################################

data4 <- ovun.sample(Attrition_Flag ~ ., data = data1, method = "over",N =13600)$data
table(data4$Attrition_Flag)

set.seed(345)
model2=train(
  Attrition_Flag~.,data = data4,method = "knn",
  trControl = trainControl("cv",number = 10),
  tuneLength = 10
)

# Make predictions on the test data
probabilities <- model2 %>% predict(data2)
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition_Flag)

# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition_Flag))

############################### No sampling techniques ##################################

set.seed(345)
model3=train(
  Attrition_Flag~.,data = data1,method = "knn",
  trControl = trainControl("cv",number = 10),
  tuneLength = 10
)

# Make predictions on the test data
probabilities <- model3 %>% predict(data2)
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition_Flag)

# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition_Flag))
