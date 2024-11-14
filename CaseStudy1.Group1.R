# Fall 2024
# Seminar in Business Analytics: Case Study 1, The Airline Industry

# Group 1: Brianna L. Palmisano, Julianna LoMonte & Ava C. Rice
# St. John’s University, The Peter J. Tobin College of Business

# Title: Predicting Flight Delays using Multiple Linear Regression

#=======================================================================
# Section 1: Linear Regression
#=======================================================================

# Step 0: Set working directory, install required packages & load the .csv file
# Set working directory
setwd("~/Documents/Capstone/Week 5")
# Load necessary packages
install.packages("caTools")
install.packages("car")
install.packages("gplots")
install.packages("glmnet")
install.packages("caret")
library(caTools)
library(car)
library(gplots)
library(glmnet)
library(caret)

# Read in the dataset
data1 <- read.csv("Chapter_06_flight_delay.csv", header= TRUE, sep= ",")
View(data1)
write.csv(data1, "dataset.csv", row.names = FALSE)  # Set row.names=FALSE to avoid saving row numbers

# ==================================================
# Step 1: Preliminary Data Cleaning...
# Find number of Carriers
num_carriers <- length(unique(data1$Carrier))
cat("Number of unique carriers:", num_carriers, "\n")
# Overview of the dataset to understand the structure
str(data1)
# Check for any missing values in the dataset
sum(is.na(data1))
# Remove categorical variables
data2 <- data1[-c(1)]  # Remove Carrier

# ==================================================
# Step 2: Exploratory Data Analysis...
# Graphical Analysis
# Scatter plots of Dependent Variable vs. Each Independent Variable
par(mfrow=c(3,3)) # Set up a 3x3 plotting panel for the independent variables
# Plot 1: Number of Flights vs. Arrival Delay 
plot(data2$Number_of_flights, data2$Arr_Delay, xlab="Number of Flights", ylab="Arrival Delay (mins)", # Use cex= to set text size
     main="Number of Flights vs. Arrival Delay", pch=16, cex=0.3) # Use pch to fill circles; 16= filled circles, 1= open, 0= squares
abline(lm(data2$Arr_Delay ~ data2$Number_of_flights), col="red", lwd=2)  # Add trendline, set width using lwd
# Plot 2: Airport Distance vs. Arrival Delay 
plot(data2$Airport_Distance, data2$Arr_Delay, xlab="Airport Distance", ylab="Arrival Delay (mins)", 
     main="Airport Distance vs. Arrival Delay", pch=16, cex=0.3)
abline(lm(data2$Arr_Delay ~ data2$Airport_Distance), col="red", lwd=2)
# Plot 3: Weather vs. Arrival Delay  
plot(data2$Weather, data2$Arr_Delay, xlab="Weather", ylab="Arrival Delay (mins)", 
     main="Weather vs. Arrival Delay", pch=16, cex=0.3)
abline(lm(data2$Arr_Delay ~ data2$Weather), col="red", lwd=2)
# Plot 4: Support Crew Availability vs. Arrival Delay
plot(data2$Support_Crew_Available, data2$Arr_Delay, xlab="Support Crew Available", ylab="Arrival Delay (mins)", 
     main="Support Crew vs. Arrival Delay", pch=16, cex=0.3)
abline(lm(data2$Arr_Delay ~ data2$Support_Crew_Available), col="red", lwd=2)
# Plot 5: Baggage Loading Time vs. Arrival Delay 
plot(data2$Baggage_loading_time, data2$Arr_Delay, xlab="Baggage Loading Time (mins)", ylab="Arrival Delay (mins)", 
     main="Baggage Loading Time vs. Arrival Delay", pch=16, cex=0.3)
abline(lm(data2$Arr_Delay ~ data2$Baggage_loading_time), col="red", lwd=2)
# Plot 6: Late Arrival vs. Arrival Delay
plot(data2$Late_Arrival_o, data2$Arr_Delay, xlab="Late Arrival (Previous Flight)", ylab="Arrival Delay (mins)", 
     main="Late Arrival vs. Arrival Delay", pch=16, cex=0.3)
abline(lm(data2$Arr_Delay ~ data2$Late_Arrival_o), col="red", lwd=2)
# Plot 7: Cleaning Time vs. Arrival Delay
plot(data2$Cleaning_o, data2$Arr_Delay, xlab="Cleaning Time (mins)", ylab="Arrival Delay (mins)", 
     main="Cleaning Time vs. Arrival Delay", pch=16, cex=0.3)
abline(lm(data2$Arr_Delay ~ data2$Cleaning_o), col="red", lwd=2)
# Plot 8: Fueling Time vs. Arrival Delay 
plot(data2$Fueling_o, data2$Arr_Delay, xlab="Fueling Time (mins)", ylab="Arrival Delay (mins)", 
     main="Fueling Time vs. Arrival Delay", pch=16, cex=0.3)
abline(lm(data2$Arr_Delay ~ data2$Fueling_o), col="red", lwd=2)
# Plot 9: Security Time vs. Arrival Delay 
plot(data2$Security_o, data2$Arr_Delay, xlab="Security Time (mins)", ylab="Arrival Delay (mins)", 
     main="Security Time vs. Arrival Delay", pch=16, cex=0.3)
abline(lm(data2$Arr_Delay ~ data2$Security_o), col="red", lwd=2)
 
# Descriptive Statistics
summary(data2$Arr_Delay)  # Summary of the Target Variable (Arrival Delay)
summary(data2)  # Summary of All Variables

# ==================================================
# Step 3: Correlation Analysis...
# Find the correlation between the variables with most linear trend visible.
# Correlation between Arrival Delay and other numerical variables
corr_num_flights <- cor.test(data2$Arr_Delay, data2$Number_of_flights, method = "pearson")
corr_airport_distance <- cor.test(data2$Arr_Delay, data2$Airport_Distance, method = "pearson")
corr_weather <- cor.test(data2$Arr_Delay, data2$Weather, method = "pearson")
corr_support_crew <- cor.test(data2$Arr_Delay, data2$Support_Crew_Available, method = "pearson")
corr_baggage <- cor.test(data2$Arr_Delay, data2$Baggage_loading_time, method = "pearson")
corr_late_arrival <- cor.test(data2$Arr_Delay, data2$Late_Arrival_o, method = "pearson")
# Print results
print(corr_num_flights)
print(corr_airport_distance)
print(corr_weather)
print(corr_support_crew)
print(corr_baggage)
print(corr_late_arrival)

# Build a Correlation Matrix to visualize relationships between ALL variables
install.packages("gplots")
library(gplots)

correlation_matrix <- round(cor(data2),3)  # Generate correlation matrix
correlation_matrix
color_palette <- colorRampPalette(c("red", "white", "blue"))(100)  # 100 color levels
heatmap.2(as.matrix(correlation_matrix), 
          col = color_palette, 
          scale = "none",                   # No scaling of the data
          trace = "none",                   # Remove trace lines
          cellnote = round(correlation_matrix, 2),  # Add the correlation numbers, rounded to 2 decimal places
          notecol = "black",                # Color of the numbers
          margins = c(8,8),                 # Adjust matrix margins
          cexRow = 0.7,                     # Adjust row label size
          cexCol = 0.7,                     # Adjust column label size
          key = FALSE,                      # Remove the color key
          Rowv = FALSE,                     # Remove row dendrogram
          Colv = FALSE)                     # Remove col dendrogram

# Correlation of ONLY significant variables
data3 <- data2[ , -c(7, 8, 9)]  # Remove the 3 unwanted variables by their column numbers (columns 7, 8, and 9)
correlation_matrix <- round(cor(data3),3)  # Generate correlation matrix
correlation_matrix
# Create the heatmap with smooth shading and numbers
color_palette <- colorRampPalette(c("red", "white", "blue"))(100)  # 100 color levels
heatmap.2(as.matrix(correlation_matrix), 
          col = color_palette,   
          scale = "none",                   # No scaling of the data
          trace = "none",                   # Remove trace lines
          cellnote = round(correlation_matrix, 2),  # Add the correlation numbers, rounded to 2 decimal places
          notecol = "black",                # Color of the numbers
          margins = c(8, 8),                # Adjust matrix margins
          cexRow = 0.7,                     # Adjust row label size
          cexCol = 0.7,                     # Adjust column label size
          key = FALSE,                      # Remove the color key
          Rowv = FALSE,                     # Remove row dendrogram
          Colv = FALSE)                     # Remove col dendrogram

# ==================================================
# Step 4: Splitting Data into Training and Testing Sets...
# This ensures that our model is trained on 70% of the data, and tested on the remaining 30%
set.seed(1000)  # Ensure reproducibility by setting seed high
sample <- sample.split(data2$Arr_Delay, SplitRatio=0.70)  # 70-30 split
train_data <- subset(data2, sample == TRUE)  # Training dataset
test_data <- subset(data2, sample == FALSE)  # Testing dataset

# ==================================================
# Step 5: Initial Linear Regression Model (using ALL independent variables)
initial_lm_model <- lm(Arr_Delay ~., data = train_data)
summary(initial_lm_model
        
# ==================================================
# ==================================================
# Step 6: Final Multiple Linear Regression Model following Variable Selection
# Final Linear Model (using ONLY significant variables)
final_lm_model <- lm(Arr_Delay ~ Airport_Distance + Number_of_flights + Weather + 
                       Support_Crew_Available + Baggage_loading_time + Late_Arrival_o, 
                     data = train_data)
summary(final_lm_model)

# ==================================================
# Step 7: Cross-Validated Linear Regression Model
library(caret)

# Set up the cross-validation method (10-fold)
train_control <- trainControl(method = "cv", number = 10)

# Train the cross-validated linear regression model
cv_linear_model <- train(Arr_Delay ~ Airport_Distance + Number_of_flights + Weather + 
                           Support_Crew_Available + Baggage_loading_time + Late_Arrival_o, 
                         data = train_data, 
                         method = "lm", 
                         trControl = train_control)

# Summary of the cross-validated model
summary(cv_linear_model)

# ==================================================
# Step 8: Linear Regression Diagnostics  
# Predicted and Residual values for training data (Final Linear Model)...
pred_train <- final_lm_model$fitted.values  # Fitted values (predictions) from the final model
resed_train <- final_lm_model$residuals  # Residuals from the final model

# Plot Actual vs. Predicted Arrival Delays
par(mfrow = c(1, 1))  # Reset to single plot layout
predicted_delays <- predict(final_lm_model, newdata = test_data)
plot(test_data$Arr_Delay, col = "red", type = "l", lty = 1.8, lwd = 2, 
     xlab = "Observation Number", ylab = "Arrival Delay (mins)",
     main = "Actual vs Predicted Arrival Delays")
lines(predicted_delays, col = "blue", type = "l", lty = 1.4, lwd = 2)

# Model validation with diagnostic plots
par(mfrow = c(2,2))
# Plot 1: Residuals vs Fitted Plot
plot(final_lm_model, which = 1, col = "blue", pch = 16, cex = 0.3)

# Plot 2: Normal Q-Q Plot
plot(final_lm_model, which = 2, col = "green", pch = 16, cex = 0.3)

# Plot 3: Scale-Location Plot
plot(final_lm_model, which = 3, col = "orange", pch = 16, cex = 0.3)

# Plot 4: Cook's Distance Plot
plot(final_lm_model, which = 5, col = "purple", pch = 16, cex = 0.3)

# VIF Test for Multicollinearity
vif_values <- vif(final_lm_model)
# Sort the VIF values from least to greatest
sorted_vif_values <- sort(vif_values)
# Print the VIF values
vif_data <- data.frame(Variable = names(sorted_vif_values),
                       VIF = sorted_vif_values,
                       Exceeds_Threshold = sqrt(sorted_vif_values) > 5) # Include threshold for VIF here
print(vif_data)

# ==================================================
# Section 2: Lasso and Ridge Regression
# ==================================================

# Step 9: Lasso and Ridge Regression with Feature Scaling and Cross-Validation
# Step 9.1: Feature Scaling (Standardize the data)
x_train <- as.matrix(train_data[, c("Airport_Distance", "Number_of_flights", "Weather", 
                                    "Support_Crew_Available", "Baggage_loading_time", "Late_Arrival_o")])
y_train <- train_data$Arr_Delay
x_test <- as.matrix(test_data[, c("Airport_Distance", "Number_of_flights", "Weather", 
                                  "Support_Crew_Available", "Baggage_loading_time", "Late_Arrival_o")])
y_test <- test_data$Arr_Delay

# Standardize the training and test data to have zero mean and unit variance
x_train_scaled <- scale(x_train)
x_test_scaled <- scale(x_test)

# Step 9.2: Lasso Regression with Cross-Validation
lambda_grid <- 10^seq(-3, .05, length = 100)  # A grid of lambda values
lasso_model <- cv.glmnet(x_train_scaled, y_train, alpha = 1, lambda = lambda_grid)
lasso_pred <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_test_scaled, type = "response")

# Extract and print Lasso coefficients at the optimal lambda
lasso_coefficients <- coef(lasso_model, s = lasso_model$lambda.min)
print("Lasso Coefficients:")
print(lasso_coefficients)

# Step 9.3: Ridge Regression with Cross-Validation
ridge_model <- cv.glmnet(x_train_scaled, y_train, alpha = 0, lambda = lambda_grid)
ridge_pred <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_test_scaled, type = "response")

# Extract and print Ridge coefficients at the optimal lambda
ridge_coefficients <- coef(ridge_model, s = ridge_model$lambda.min)
print("Ridge Coefficients:")
print(ridge_coefficients)

# ==================================================
# Step 10: Model Diagnostics and RSE Calculation on Test Set
# Function to calculate RSE manually from actual and predicted values
calculate_rse_manual <- function(actual, predicted, model) {
  n <- length(actual)  # Number of observations
  p <- length(coef(model))  # Number of predictors
  residuals <- actual - predicted
  rse <- sqrt(sum(residuals^2) / (n - p))
  return(rse)
}

# RSE for Initial Linear Model
initial_model_pred <- predict(final_lm_model, newdata = test_data)
rse_initial <- calculate_rse_manual(test_data$Arr_Delay, initial_model_pred, final_lm_model)

# RSE for Cross-Validated Linear Model
cv_linear_pred <- predict(cv_linear_model, newdata = test_data)
rse_cv_linear <- calculate_rse_manual(test_data$Arr_Delay, cv_linear_pred, cv_linear_model$finalModel)

# RSE for Lasso Model
rse_lasso <- sqrt(mean((y_test - lasso_pred)^2))

# RSE for Ridge Model
rse_ridge <- sqrt(mean((y_test - ridge_pred)^2))

# Model Comparison Results
results <- data.frame(
  Model = c("Initial Linear Model", "CV Linear Model", "Lasso Model", "Ridge Model"),
  RSE = c(rse_initial, rse_cv_linear, rse_lasso, rse_ridge)
)
print(results)

# ==================================================
# Step 11: Actual vs. Predicted for Lasso
# Plot Actual vs Predicted Arrival Delays (Lasso)
plot(y_test, lasso_pred, 
     main = "Actual vs. Predicted Arrival Delays (Lasso)", 
     xlab = "Actual Arrival Delay (mins)", 
     ylab = "Predicted Arrival Delay (mins)", 
     pch = 16, col = "blue", cex = 0.5)
abline(a = 0, b = 1, col = "red", lwd = 2)

# ==================================================
# Step 12: Model Diagnostics for Lasso
# Predict fitted values using the Lasso model (with lambda.min for best lambda)
lasso_fitted <- predict(lasso_model, newx = x_train_scaled, s = lasso_model$lambda.min)
# Calculate residuals (difference between actual and predicted values)
lasso_residuals <- y_train - lasso_fitted  # Residuals for training data

# Identify the top 3 largest residuals
top3_residuals <- order(abs(lasso_residuals), decreasing = TRUE)[1:3]  # Top 3 largest residuals by absolute value

# Set up a 2x2 grid layout for the diagnostic plots
par(mfrow = c(2, 2))  # 2x2 layout

# Plot 1: Residuals vs Fitted (Lasso)
plot(lasso_fitted, lasso_residuals, col = "blue", pch = 16, cex = 0.3, 
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted for Lasso")
lines(lowess(lasso_fitted, lasso_residuals), col = "red", lwd = 2)
text(lasso_fitted[top3_residuals], lasso_residuals[top3_residuals], 
     labels = top3_residuals, cex = 0.8, col = "black")

# Plot 2: Normal Q-Q Plot for Lasso
qqnorm(lasso_residuals, col = "green", pch = 16, cex = 0.3, main = "Normal Q-Q Plot for Lasso")
qqline(lasso_residuals, col = "red", lwd = 2)
qq_vals <- qqnorm(lasso_residuals, plot.it = FALSE)
text(qq_vals$x[top3_residuals], qq_vals$y[top3_residuals], 
     labels = top3_residuals, cex = 0.8, col = "black")

# Plot 3: Scale-Location Plot for Lasso
scale_location <- sqrt(abs(lasso_residuals))
plot(lasso_fitted, scale_location, col = "orange", pch = 16, cex = 0.3, 
     xlab = "Fitted Values", ylab = "Sqrt(Standardized Residuals)",
     main = "Scale-Location Plot for Lasso")
lines(lowess(lasso_fitted, scale_location), col = "red", lwd = 2)
text(lasso_fitted[top3_residuals], scale_location[top3_residuals], 
     labels = top3_residuals, cex = 0.8, col = "black")

# Plot 4: Residuals (Cook's Distance Approximation) for Lasso
plot(lasso_residuals, col = "purple", pch = 16, cex = 0.3, 
     xlab = "Observation", ylab = "Residuals", 
     main = "Residuals (Cook's Distance Approximation)")
text(top3_residuals, lasso_residuals[top3_residuals], 
     labels = top3_residuals, cex = 0.8, col = "black")

# END OF CODE. THANK YOU! :) 

