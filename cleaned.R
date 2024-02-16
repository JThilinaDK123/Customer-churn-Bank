## Load the data set
dat=read.csv("BankChurners.csv")
dim(dat)

## Remove duplicates
dat<-dat[!duplicated(dat$CLIENTNUM),]
dim(dat) #no duplicates
summary(dat)

## Dropping unwanted columns
drop_cols=c("CLIENTNUM",
            "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1",
            "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2")
dat[drop_cols]=NULL
summary(dat)
head(dat)

## Factorized
dat$Attrition_Flag=factor(dat$Attrition_Flag,
                          levels = c("Existing Customer","Attrited Customer"),
                          labels = c("Existing","Attrited"))
cols=c("Gender","Education_Level","Marital_Status","Income_Category","Card_Category")
dat[cols] <- lapply(dat[cols], factor)

dim(dat)
summary(dat$Income_Category)

write.csv(dat, file="cleaned_data.csv", row.names = FALSE)


## Divide the data set into training and testing
library(tidyverse)
library(caret)

prop <- dat %>% select(Attrition_Flag) %>% group_by(Attrition_Flag) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

set.seed(111)
trainIndex <- createDataPartition(dat$Attrition_Flag, p=0.8, 
                                  list=FALSE, times=1)
train.data <- dat[trainIndex,]
test.data<- dat[-trainIndex,]

write.csv(train.data, file="train.csv", row.names = FALSE)
dim(train.data)
write.csv(test.data, file="test.csv", row.names = FALSE)
dim(test.data)

train.data %>% select(Attrition_Flag) %>% group_by(Attrition_Flag) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

test.data %>% select(Attrition_Flag) %>% group_by(Attrition_Flag) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))
