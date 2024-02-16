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
library(rpart)
library(ggplot2)

## Load the dataset
df = read.csv("cleaned_data.csv")
head(df)
summary(df)

cols=c("Gender","Education_Level","Marital_Status","Income_Category","Card_Category")
df[cols] <- lapply(df[cols], factor)
df[cols] <- lapply(df[cols], factor)
head(df)


# Card Type 
count_table <- table(df$Card_Category)
count_df <- as.data.frame(count_table)
colnames(count_df)[colnames(count_df) == "Var1"] <- "Card_Type"
count_df$Percentage <- count_df$Freq / sum(count_df$Freq) * 100
ggplot(count_df, aes(x = Card_Type, y = Percentage, fill = Card_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  xlab("Categories") +
  ylab("Percentage") +
  ggtitle("Card Type") +
  theme_minimal()

# Attrition
count_table <- table(df$Attrition_Flag)
count_df <- as.data.frame(count_table)
colnames(count_df)[colnames(count_df) == "Var1"] <- "Attrition_Type"
count_df$Percentage <- count_df$Freq / sum(count_df$Freq) * 100
ggplot(count_df, aes(x = Attrition_Type, y = Percentage, fill = Attrition_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  xlab("Categories") +
  ylab("Percentage") +
  ggtitle("Customer Activity") +
  theme_minimal()

# Attrition vs TTA
ggplot(df, aes(x = Attrition_Flag, y = Total_Trans_Amt)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Value") +
  ggtitle("Attrition type vs Total Transaction Amount")

# Attrition vs Total_Relationship_Count
ggplot(df, aes(x = Attrition_Flag, y = Total_Relationship_Count)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Value") +
  ggtitle("Attrition type vs Total No of products held by the customer")

# Attrition vs Contacts_Count_12_mon
ggplot(df, aes(x = Attrition_Flag, y = Contacts_Count_12_mon)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Value") +
  ggtitle("Attrition type vs No of contacts in the last 12 months")

# Attrition vs Months_Inactive_12_mon
ggplot(df, aes(x = Attrition_Flag, y = Months_Inactive_12_mon)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Value") +
  ggtitle("Attrition type vs No of months inactive (in last 12 months)")

# Attrition vs Total_Revolving_Bal
ggplot(df, aes(x = Attrition_Flag, y = Total_Revolving_Bal)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Value") +
  ggtitle("Attrition type vs Total Revolving Balance")

# Attrition vs Total_Ct_Chng_Q4_Q1
ggplot(df, aes(x = Attrition_Flag, y = Total_Ct_Chng_Q4_Q1)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Value") +
  ggtitle("Attrition type vs Change in Transaction Count")

# Customer activity vs totatl transaction count
ggplot(df, aes(x = Total_Trans_Ct, fill = Attrition_Flag)) +
  geom_density(alpha = 0.5) +
  xlab("Value") +
  ylab("Density") +
  ggtitle("Customer activity vs totatl transaction count")


# Total_Trans_Amt vs Total_Trans_Ct
ggplot(df, aes(x = Total_Trans_Amt, y = Total_Trans_Ct)) +
  geom_point() +
  labs(title = "Scatterplot", x = "Total_Trans_Amt", y = "Total_Trans_Ct")



# Customer type vs Income category
percentage_data <- df %>%
  group_by(Attrition_Flag, Income_Category) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

pie_charts <- ggplot(percentage_data, aes(x = "", y = percentage, fill = Income_Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Attrition_Flag) +
  labs(fill = "Education Level") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage))), position = position_stack(vjust = 0.5))

print(pie_charts)

# Gender vs Income category
percentage_data <- df %>%
  group_by(Gender, Income_Category) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

pie_charts <- ggplot(percentage_data, aes(x = "", y = percentage, fill = Income_Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Gender) +
  labs(fill = "Education Level") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage))), position = position_stack(vjust = 0.5))

print(pie_charts)







