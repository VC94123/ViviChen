getwd()
setwd("/Users/vivi/Desktop/R Programing /A3")
install.packages("dplyr")
library(dplyr)

# Overview
# Maintaining a competitive edge in the ever-changing financial services industry requires an understanding of client behavior and the ability to predict turnover. 
# Two datasets contain comprehensive records of current bank clients, encompassing personal data (age, gender, location), financial activity (balance, goods utilized, credit score), and churn status during a certain time frame. This dataset is a key resource for figuring out trends and factors that influence customer attrition, allowing for more focused retention initiatives.
# to use logistic regression and statistics to forecast client attrition, allowing proactive engagement tactics to increase customer retention.

# Load Bank churn  & New Customer dataset
bank <- read.csv("BankChurnDataset.csv")
new_customer <- read.csv("NewCustomerDataset.csv")


# 1. Handle missing values in the datasets

# Display the first 50 rows and the structure of the two datasets
head(bank,50)
str(bank)
head(new_customer,50)
str(new_customer)

# Removing unwanted columns

df.bank <- select(bank,-CustomerId,-Surname,-id)
df.customer <- select(new_customer,-id,-CustomerId,-Surname)

# Checking remaining columns 
head(df.bank,3)
head(df.customer,3)

# Check for missing values in two datasets
sum(is.na(df.bank)) #9 missing value
sum(is.na(df.customer)) #0 missing value

# Locate missing data in the bank dataset
df.bank[!complete.cases(df.bank),]

# Prepare the dataset for analysis by selecting columns and converting data types
# (Includes converting strings to factors.)

df.bank$Geography <-as.factor(df.bank$Geography)
df.bank$Gender <-as.factor(df.bank$Gender)
df.bank$HasCrCard <-as.factor(df.bank$HasCrCard)
df.bank$IsActiveMember <-as.factor(df.bank$IsActiveMember)
df.bank$Exited <-as.factor(df.bank$Exited)

# Checking structure
str(df.bank)

df.customer$Geography <-as.factor(df.customer$Geography)
df.customer$Gender <-as.factor(df.customer$Gender)
df.customer$HasCrCard <-as.factor(df.customer$HasCrCard)
df.customer$IsActiveMember <-as.factor(df.customer$IsActiveMember)


# Checking structure
str(df.customer)


# Boxplot of Age by Gender for bank dataset, for find median value to fill Na Value in Age & EstimatedSalary
library(ggplot2)

ggplot(df.bank, aes(x = Gender, y = Age)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Age by Gender", x = "Gender", y = "Age")

# Calculate the median (less sensitive to outliers and maintains the data distribution) of male & female data, then fill in the NA value
male_data <- subset(df.bank, Gender == "Male")
median_age_male <- median(male_data$Age, na.rm = TRUE) #37
median_age_male
df.bank[is.na(df.bank$Age)&df.bank$Gender=="Male",]
df.bank[is.na(df.bank$Age)&df.bank$Gender=="Male","Age"]<- median_age_male


female_data <- subset(df.bank, Gender == "Female")
median_age_female <- median(female_data$Age, na.rm = TRUE)#38
median_age_female
df.bank[is.na(df.bank$Age)&df.bank$Gender=="Female",]
df.bank[is.na(df.bank$Age)&df.bank$Gender=="Female","Age"]<- median_age_female


ggplot(df.bank, aes(x = Gender, y = EstimatedSalary)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Estimated Salary by Gender", x = "Gender", y = "Estimated Salary")

median_salary_male <- median(male_data$EstimatedSalary, na.rm = TRUE) #117122.5
median_salary_male
df.bank[is.na(df.bank$EstimatedSalary)&df.bank$Gender=="Male",]
df.bank[is.na(df.bank$EstimatedSalary)&df.bank$Gender=="Male","EstimatedSalary"]<- median_salary_male


# 2.Divide the "BankChurnDataset" into the training and testing dataset. 
# Train the model and calculate Model Accuracy, specificity, and sensitivity of the dataset.


install.packages("caTools")
library(caTools)
set.seed(101)
# Use 70/30 split data to train & test
split = sample.split(df.bank$Exited, SplitRatio = 0.70)
train = subset(df.bank, split == TRUE)
test = subset(df.bank, split == FALSE)

log.model <- glm(formula=Exited ~ . , family = binomial(link='logit'),data = train) #glm= generalized linear model

#Understand the relationships between predictors and the response variable in logistic regression.
summary(log.model)
# For example:CreditScore: For each unit increase in credit score, the log-odds of churn decreases by 0.0006539, 
# statistically significant (p = 2.94e-10), suggesting that higher credit scores are associated with lower churn probability.

#Check Prediction accuracy

fitted.probabilities <- predict(log.model,newdata=test,type='response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0) # if greater than 0.5, p is 1.(0: won't exited, 1: exited)

# mean() function to a logical vector in R, it treats TRUE as 1 and FALSE as 0. 
# Therefore, the mean() function calculates the proportion of TRUE values in the 
# logical vector, which is equivalent to the percentage of TRUE values.

misClasificError <- mean(fitted.results != test$Exited)
print(paste('Accuracy',1-misClasificError)) # Accuracy = 1 - Misclassification 

# The model accurately predicts 83.43% of the time whether a customer would churn or not. 


# Creating Confusion Matrix, in order to compare the actual results (test$Exited) with the model's predictions (fitted.results) 
# the table function generates a confusion matrix

table(test$Exited, fitted.results)

# True Negatives (TN): 37,285 is the number of customers that the model accurately estimated would not experience attrition.
# False Positives (FP): 1,749, denoting the customers who did not experience churn but were mispredicted by the model to do so.
# False Negatives (FN): 6,457, the quantity of clients who really left but were mispredicted not to leave.
# True Positives (TP): 4,019 indicates the consumers whose churn the model accurately anticipated.


install.packages("caret")
library(caret)

# Changing test$Exited and fitted into factor form to calculate sensitivity & specificity
test$Exited <- as.factor(test$Exited)
fitted.results <- as.factor(fitted.results)

sensitivity(test$Exited, fitted.results) # 0.8523844
specificity(test$Exited, fitted.results) # 0.6967753

# Sensitivity: The model correctly identified 85.24% of the customers who exited.This is essential for carrying out focused treatments to keep these clients. 
# Specificity: The model correctly identified 69.68% of the customers who did not exit, as evidenced by the lower specificity when compared to sensitivity,
# this can cause needless retention efforts to be focused on clients who weren't actually in danger of leaving.


# 3. Use the model to predict whether a particular customer would churn or not using the "NewCustomerDataset".

# Generate predictions for the new customer data using the logistic regression model. 
new_customer_predictions <- predict(log.model,newdata=df.customer,type='response')
# A new column is added to the df.customer data frame, PredictedExited, which will contain the predicted classification based on the model's probabilities. 
df.customer$PredictedExited <- ifelse(new_customer_predictions > 0.5, 1, 0)
df.customer$PredictedExited # Shows the predicted classifications for all customers in the new data frame
#Calculates the average of the PredictedExited column, which is the proportion of customers predicted to churn. 
churn_rate <- mean(df.customer$PredictedExited) 
# The churn rate calculated in the previous step is multiplied by 100 to convert it from a proportion to a percentage. 
print(paste("predict churn rate：", churn_rate * 100, "%"))

# The logistic regression model was applied to new customer data to predict churn probabilities. 
# These probabilities were then classified into binary outcomes(the logic are same us the above one), 
# the model suggests a predicted churn rate of approximately 11.7% for the new customers.


# Generate the barplot show Predicted Exited Proportion by Geography and Gender, and by Num Of Products
# Create a data frame for plotting
plot_data <- data.frame(
  Status = c("Not Churned", "Churned"),
  Count = c(sum(df.customer$PredictedExited == 0), sum(df.customer$PredictedExited == 1))
)

ggplot(df.customer, aes(x=Geography, fill=factor(PredictedExited))) +
  geom_bar() +
  facet_wrap(~ Gender) +
  labs(title="Predicted Exited Proportion by Geography and Gender",
       x="Location",
       y="Proportion of Predicted Exited",
       fill="Exited Or Not") 

ggplot(df.customer, aes(x = NumOfProducts, fill = factor(PredictedExited))) +
  geom_bar() +
  labs(title = "Predicted Exited Proportion by Num Of Products and Predicted Exited",
       x = "Num Of Products",
       y = "Proportion of Predicted Exited",
       fill = "Exited Or Not")

# We can see that the count of consumers anticipated to churn (coral section) is often fewer than those predicted to stay (red segment). 
# In contrast to France and Spain, there is a discernible difference in Germany, where the coral segment is significantly larger and implies a higher churn prediction for both genders.
# The percentage of consumers expected to stay is higher than the percentage expected to exit in each of the three countries.
# Within the same nation, there doesn't seem to be much of a gender variation in the percentage of expected churn.
# For both genders, Germany exhibits a greater predicted exited rate than France and Spain.
# Here are some suggestions:
# 1.	Concentrated Efforts in Germany: Because of the higher anticipated churn rates, give Germany top priority for retention efforts. 
# Adopt specialized tactics, reward schemes, and merchandise that meets the needs of German consumers.
# 2.	New Strategies: Make better use of product offers, communication, and support tactics to improve customer retention.
# Also can set up the strategies by different gender, for example: Woman's products(credit cards) or promotions.
# 3.	Feedback and Incentives: Establish a methodical procedure for gathering customer feedback, 
# and create incentive plans that use exclusive deals and discounts to entice high-risk clients(for example: only have one product clients) who are about to leave.


