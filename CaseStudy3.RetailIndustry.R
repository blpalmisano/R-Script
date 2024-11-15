
# Capstone/Seminar, Case Study 3
# Sales Forecasting in the Retail Industry via SARIMA Model Approach

# Workflow Steps:
#   0. Data Preparation: Load and convert data to a time series (ts) format with monthly frequency.
#   1. Exploratory Data Analysis: Visualize the data, check for missing values, and assess trends and seasonality.
#   2. Stationarity Check: Perform the Dickey-Fuller test to determine if differencing is needed.
#   3. Model Identification: Use ACF/PACF plots and differencing to identify potential SARIMA model orders.
#   4. Initial Model Estimation: Build and evaluate two SARIMA models with selected parameters.
#   5. Model Comparison: Compare models using AIC to identify the better-performing model.
#   6. Automatic Model Refinement: Use auto.arima to optimize the selected model’s parameters.
#   7. Forecasting: Generate and plot sales forecasts with confidence intervals using the optimized model.

# Set working directory
setwd("~/Documents/Capstone/Week 7")

# Install & load packages...
install.packages("astsa")
install.packages("forecast")
install.packages("tseries")
library(astsa)
library(forecast)
library(tseries)

# --- Step 0: Load Data and Convert to Time Series
data1 <- read.csv("Chapter_03.csv", header=TRUE, sep=",")
data <- ts(data1[,2], start = c(1992,1), frequency = 12) # Monthly data

# --- Step 1: Exploratory Data Analysis
# Display start and end date of the series, frequency, and data type
start(data)
end(data)
frequency(data)
class(data)

# Check for missing values and provide descriptive statistics
sum(is.na(data))
summary(data)

# Plot the time series to visualize trends and seasonality
plot(data, xlab='Years', ylab='Sales', col="red", main="SALES", pch=16, cex=0.3)

# --- Step 2: Stationarity Check with Dickey-Fuller Test
# Conduct Augmented Dickey-Fuller test to check for stationarity
adf_results <- adf.test(data)
print(adf_results)

# Interpretation: If p-value < 0.05, data is likely stationary; > 0.05 suggests non-stationarity

# --- Step 3: Identification Stage (Differencing, ACF/PACF analysis)
# ACF and PACF plots to identify seasonality and trend in raw data
acf2(data, max.lag = 24)

# Apply seasonal differencing (lag of 12) to remove seasonality
datadiff12 <- diff(data, 12)
plot.ts(datadiff12)

# Apply first differencing to remove trend from seasonally differenced data
diff1and12 <- diff(datadiff12, 1)
plot(diff1and12)

# Examine ACF and PACF of differenced data for model order selection
acf2(diff1and12, max.lag = 36)

# --- Step 4: Initial Model Estimation & Diagnostic Checking
# Model 1: SARIMA(2,1,1)(2,1,2)[12]
model1 <- arima(data, order = c(2,1,1), seasonal = list(order = c(2,1,2), period = 12))
summary(model1)

# Check residuals for white noise in Model 1
Acf(residuals(model1))
Box.test(residuals(model1), lag=24, fitdf=1, type="Ljung")

# Model 2: SARIMA(6,1,1)(2,1,2)[12]
model2 <- arima(data, order = c(6,1,1), seasonal = list(order = c(2,1,2), period = 12))
summary(model2)

# Check residuals for white noise in Model 2
Acf(residuals(model2))
Box.test(residuals(model2), lag=24, fitdf=1, type="Ljung")

# --- Step 5: Model Comparison and Selection
# Compare AIC values to select the better model
aic_model1 <- AIC(model1)
aic_model2 <- AIC(model2)
cat("AIC for Model 1 (SARIMA(2,1,1)(2,1,2)[12]):", aic_model1, "\n")
cat("AIC for Model 2 (SARIMA(6,1,1)(2,1,2)[12]):", aic_model2, "\n")

if (aic_model1 < aic_model2) {
  better_model <- model1
  cat("Model 1 is selected as the better model based on AIC.\n")
} else {
  better_model <- model2
  cat("Model 2 is selected as the better model based on AIC.\n")
}

# --- Step 6: Automatic ARIMA Based on Better Model
# Use auto.arima to refine the better model selected in Step 5
auto_model <- auto.arima(data, start.p = better_model$arma[1], start.q = better_model$arma[2],
                         start.P = better_model$arma[5], start.Q = better_model$arma[6],
                         seasonal = TRUE)
summary(auto_model)

# --- Step 7: Forecasting Stage with Automatic Model
# --- Forecasting with Model 2 (Manual SARIMA(6,1,1)(2,1,2)[12])
# Generate a 30-month forecast using Model 2
model2_forecast <- forecast(model2, h = 30)

# Display the forecasted values with confidence intervals
print(model2_forecast)

# Plot the forecast results
plot(model2_forecast, ylab="Sales (million in dollars)", xlab="Year", 
     main="Forecast with Manual SARIMA Model 2")

# ---

# Forecast sales for the next 30 months based on the automatic ARIMA model
auto_forecast <- forecast(auto_model, h = 30)
print(auto_forecast)

# Plot forecast results
plot(auto_forecast, ylab="Sales (million in dollars)", xlab="Year", main="Auto ARIMA Forecast Model 2", fcol="red")

# ---

# Plot the forecasts together for comparison
plot(model2_forecast, ylab="Sales (million in dollars)", xlab="Year", 
     main="Comparison of Forecasts: Manual SARIMA Model 2 vs. Automatic ARIMA", 
     col=c("black", "blue"), lwd=2, xlim=c(2015, max(time(auto_forecast$mean))))  # Limit x-axis to start at 2015
# Add the forecast from the Automatic ARIMA model to the same plot
lines(auto_forecast$mean, col="red", lwd=2)  # Plot Automatic ARIMA in red

# Add legend for clarity
legend("bottomright", legend=c("Manual SARIMA Model 2", "Automatic ARIMA"), 
       col=c("blue", "red"), lty=1, lwd=2)


# END CODE. 
