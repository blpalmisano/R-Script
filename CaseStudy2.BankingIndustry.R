# Group 1
# Capstone/Seminar: Case Study #2
# Predicting Loan Default Using Logistic Regression

# Set working directory...
setwd("~/Documents/Capstone/Week 6")

# Load packages & libraries...
install.packages("car")
install.packages("MKmisc")
install.packages("ResourceSelection")
install.packages("lmtest")
install.packages("survey")
install.packages("ROCR")
install.packages("reshape2")
install.packages("ggplot2")

library(car)
library(MKmisc)
library(ResourceSelection)
library(lmtest)
library(survey)
library(ROCR)
library(reshape2)
library(ggplot2)

# Load data file...
data1 <- read.csv ("loan_default.csv",header=TRUE,sep=",")
data2<-data.frame(data1)
data3 <- data2
View(data2)

# View variables names and type... 
names(data2)
str(data2) 

# Check of missing or 'NA' values...
sum(is.na(data2)) 

# Run Descriptive Statistics...
# For continuous and descrete variables (excluding/dropping binary and categorical variables, here)
selected_data <- data2[, c("Checking_amount", "Term", "Credit_score", "Amount", 
                           "Saving_amount", "Emp_duration", "Age", "No_of_credit_acc")]
summary(selected_data)


# --- Assumption Check: Linearity of Predictors with Log-Odds
# Create a log-odds transformation for Default (target variable), to check linearity
# In Linear Regression, the indep. and dep. must be linear, but...
# In Logistice Regression, the indep. mut be linear with log-odd of the dep., not the dep.
data3$log_odds_default <- log((data3$Default + 0.001) / (1 - data3$Default + 0.001))

# Predictor vs. Log-Odds scatter plots
# Set layout for 4x2 grid of plots
par(mfrow = c(4, 2))

# Plot 1: Log-Odds of Default vs. Checking Amount
plot(data3$Checking_amount, data3$log_odds_default, 
     xlab="Checking Amount ($)", ylab="Log-Odds of Default", 
     main="Log-Odds vs. Checking Amount", pch=16, cex=0.3)
lines(lowess(data3$Checking_amount, data3$log_odds_default), col="red", lwd=2)

# Plot 2: Log-Odds of Default vs. Loan Term
plot(data3$Term, data3$log_odds_default, 
     xlab="Loan Term (Months)", ylab="Log-Odds of Default", 
     main="Log-Odds vs. Loan Term", pch=16, cex=0.3)
lines(lowess(data3$Term, data3$log_odds_default), col="red", lwd=2)

# Plot 3: Log-Odds of Default vs. Credit Score
plot(data3$Credit_score, data3$log_odds_default, 
     xlab="Credit Score", ylab="Log-Odds of Default", 
     main="Log-Odds vs. Credit Score", pch=16, cex=0.3)
lines(lowess(data3$Credit_score, data3$log_odds_default), col="red", lwd=2)

# Plot 4: Log-Odds of Default vs. Loan Amount
plot(data3$Amount, data3$log_odds_default, 
     xlab="Amount ($)", ylab="Log-Odds of Default", 
     main="Log-Odds vs. Loan Amount", pch=16, cex=0.3)
lines(lowess(data3$Amount, data3$log_odds_default), col="red", lwd=2)

# Plot 5: Log-Odds of Default vs. Saving Amount
plot(data3$Saving_amount, data3$log_odds_default, 
     xlab="Saving Amount ($)", ylab="Log-Odds of Default", 
     main="Log-Odds vs. Saving Amount", pch=16, cex=0.3)
lines(lowess(data3$Saving_amount, data3$log_odds_default), col="red", lwd=2)

# Plot 6: Log-Odds of Default vs. Employment Duration
plot(data3$Emp_duration, data3$log_odds_default, 
     xlab="Employment Duration (Years)", ylab="Log-Odds of Default", 
     main="Log-Odds vs. Employment Duration", pch=16, cex=0.3)
lines(lowess(data3$Emp_duration, data3$log_odds_default), col="red", lwd=2)

# Plot 7: Log-Odds of Default vs. Age
plot(data3$Age, data3$log_odds_default, 
     xlab="Age (Years)", ylab="Log-Odds of Default", 
     main="Log-Odds vs. Age", pch=16, cex=0.3)
lines(lowess(data3$Age, data3$log_odds_default), col="red", lwd=2)

# Plot 8: Log-Odds of Default vs. Number of Credit Accounts
plot(data3$No_of_credit_acc, data3$log_odds_default, 
     xlab="Number of Credit Accounts", ylab="Log-Odds of Default", 
     main="Log-Odds vs. No. of Credit Accounts", pch=16, cex=0.3)
lines(lowess(data3$No_of_credit_acc, data3$log_odds_default), col="red", lwd=2)

# --- Correlation Analysis
# Correlation Matrix for continuous variables
# Create a subset of continuous variables, including Default logg odds
# Using Log odds is more accurate, but made little difference compared to just using default
continuous_vars <- data3[, c("log_odds_default", "Amount", "Term", "Credit_score", "Saving_amount", "Age", "Emp_duration", "No_of_credit_acc")]
cor_matrix <- cor(continuous_vars, method = "pearson")
print(cor_matrix)

# Convert the correlation matrix to long format for ggplot
cor_data <- melt(cor_matrix)

# Plotting the heatmap with ggplot2
ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "darkgreen", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  geom_text(aes(label = round(value, 3)), color = "black", size = 3) +  # Add values with rounding
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1), plot.title = element_text(size = 10)) +
  coord_fixed() +
  labs(title = "Correlation Matrix Heatmap for Continuous Variables", x = "", y = "")


# Correlation between Default and Term
corr <- cor.test(data2$Default, data2$Term, method = "pearson")
corr  # Display correlation coefficient and p-value

# Step 2: List of Continuous Variables for Correlation Testing
continuous_vars <- c("Amount", "Term", "Credit_score", "Saving_amount", "Age", "Emp_duration", "No_of_credit_acc")

# Step 3: Initialize an empty data frame to store results
cor_results <- data.frame(
  Predictor = character(),
  Correlation = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Step 4: Loop through each continuous variable and run cor.test with log_odds_default
for (var in continuous_vars) {
  test_result <- cor.test(data2$Default, data2[[var]], method = "pearson")
  
  # Append the results to cor_results data frame
  cor_results <- rbind(cor_results, data.frame(
    Predictor = var,
    Correlation = round(test_result$estimate, 4),
    P_Value = round(test_result$p.value, 4)
  ))
}
# Display the results in a formatted table
print(cor_results)



# --- Initial Logistic Regression Model with All Variables
fullmodel1 <- glm(Default ~ ., data = data2, family = binomial(link = "logit"))
summary(fullmodel1)

# --- Model Refinement (only significant predictors)
fullmodel2 <- glm(Default ~ Checking_amount + Term + Credit_score + Emp_status +
                    Saving_amount + Age, data = data2, family = binomial(link = "logit"))
summary(fullmodel2)

library(survey)
# --- Perform Wald Test on Each Predictor in the Final Model
# Initialize a data frame to store Wald test results
regTermTest(fullmodel2,pred)

# List only the unique main predictor names
predictors <- c("Checking_amount", "Term", "Credit_score", "Saving_amount", "Age")
# Initialize the results data frame
wald_results <- data.frame(
  Predictor = character(),
  Wald_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)
# Loop through each predictor and perform Wald test
for (pred in predictors) {
  test_result <- regTermTest(fullmodel2, pred)
  
  # Append results to the results data frame
  wald_results <- rbind(wald_results, data.frame(
    Predictor = pred,
    Wald_Statistic = round(test_result$F, 4),
    P_Value = round(test_result$p, 4)
  ))
}
# Display the results
print(wald_results)



# --- Data Splitting into Training and Testing Sets
set.seed(2)  # For reproducibility
train_obs <- floor(0.7 * nrow(data2))
train_ind <- sample(seq_len(nrow(data2)), size = train_obs)
train_data <- data2[train_ind, ]  # Training set
test_data <- data2[-train_ind, ]  # Testing set

# Model on training (All variables)
model1 <- glm(Default ~ ., data = train_data, family = binomial(link = "logit"))
summary(model)

# Final refined model on training data
model2 <- glm(Default ~ Checking_amount + Term + Credit_score + Emp_status + Saving_amount + Age, data = train_data, family = binomial(link = "logit"))
summary(model2)


library(car)
library(MKmisc)
library(ResourceSelection)
library(lmtest)
library(survey)
library(caret)
library(cluster)
# --- Multicollinearity Check with Variance Inflation Factor (VIF)
vif(model2)  # Ensure VIF values are acceptable (< 5-10)

# Predicting the model using test data
Prob <-predict(model2,test_data,type ="response")
prob2 <- predict(model2,train_data,type ="response")
prob1 <- data.frame(Prob)
prob2 <- data.frame(prob1)

# setting the cutoff for probability values
results1 <- ifelse(prob1 > 0.7,1,0)
results2 <- ifelse(prob2 > 0.7,1,0) #training data

#Display the confusion matrix or classification table
testing_high <- test_data$Default
table(testing_high,results1)

#Calculating the error rate
misclasificationerror <- mean(results1 != testing_high) 
misclasificationerror
sum(results1 != testing_high) # Number of TRUE

# Calculating the accuracy rate	
accuracyrate <- 1-misclasificationerror
print(accuracyrate)


library(car)
library(MKmisc)
library(ResourceSelection)
library(lmtest)
library(survey)
library(caret)
library(cluster)
# --- Model Fit Tests
# Hosmer-Lemeshow Test for Model Fit
library(ResourceSelection)
hoslem.test(train_data$Default, fitted(model2), g = 10)

# Likelihood Ratio Test
library(lmtest)
lrtest(model1, model2)  # Compare the initial and refined models


library(ROCR)
# --- ROC Curve and AUC for Predictive Performance Validation
# Generate prediction object for ROC
pred <- prediction(Prob, test_data$Default)
pmf <- performance(pred, measure = "tpr", x.measure = "fpr")
par(mfrow=c(1,1))
# Plot ROC curve
plot(pmf, col = "red", main="ROC Curve for Test Data")

# Calculate and display AUC
auc <- performance(pred, measure = "auc")
auc_value <- auc@y.values[[1]]
print(auc_value)

