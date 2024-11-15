# 📈 Case Study #3: Sales Forecasting in the Retail Industry Using SARIMA

## Overview
This repository contains materials for a case study on sales forecasting for Glen Food & Beverage, Co., a retailer facing seasonal fluctuations in consumer demand. By applying the Seasonal ARIMA (SARIMA) model in R, the study captures monthly sales patterns to optimize inventory, staffing, and financial planning. The project also includes a comparison with Auto ARIMA to evaluate model performance and guide Glen Retailers in selecting the best forecasting method for operational planning. This work was completed for the **Seminar in Business Analytics** course at **St. John’s University, The Peter J. Tobin College of Business**.

### Authors
- **Brianna Palmisano**
- **Ava C. Rice**
- **Julianna LoMonte**

### Course
- **Course Title**: Seminar in Business Analytics, Business Analytics & Information Systems Department
- **Professor**: Dr. Michael D. Herley, D.B.A.
- **Institution**: St. John’s University, The Peter J. Tobin College of Business
- **Semester**: Fall 2024

---

## Repository Contents
- **PowerPoint Presentation**  
  - `CaseStudy3.RetailIndustry.pptx`: Covers the retail industry background, SARIMA model methodology, model evaluation, and recommendations.

- **R Script**  
  - `CaseStudy3.RetailIndustry.R`: Script for data preprocessing, SARIMA modeling, diagnostics, and forecasting comparisons.

- **Dataset**  
  - `Chapter_03.csv`: Monthly sales data from 1992 to 2017, totaling 309 observations, used to identify seasonal and trend components.

---

## Project Structure

### 1. Introduction & Industry Context  
   The retail industry experiences significant seasonal variations due to holidays, economic cycles, and market trends. Accurate forecasting enables retailers like Glen Food & Beverage to meet demand, optimize resources, and reduce inventory costs. This study aims to predict monthly sales for Glen using SARIMA, tailored for data with seasonal and non-seasonal trends.

### 2. Exploratory Data Analysis  
   Visual examination of the sales data revealed an upward trend and seasonal peaks, validating SARIMA’s suitability. Summary statistics, including mean and median sales, show right-skewed distribution, likely influenced by holiday spikes and promotional events.

### 3. SARIMA Model Development  
   The SARIMA model accounts for both trend and seasonality, with parameters:
   - **Non-seasonal terms (p, d, q)**: Represent past sales dependencies, differencing for stationarity, and error correction.
   - **Seasonal terms (P, D, Q)**: Capture recurring annual patterns over a 12-month period.
   - **Model Selection**: Model 2 (SARIMA(6,1,1)(2,1,2)[12]) was chosen based on AIC and residual diagnostics, outperforming Model 1.

### 4. Model Evaluation & Diagnostics  
   Model diagnostics confirmed that Model 2 achieved stationarity and minimal residual autocorrelation, as indicated by the Box-Ljung test. The lower AIC score (4600.89) and clean residuals support the model's fit for forecasting Glen’s seasonal sales.

### 5. Forecasting Model Comparison  
   A comparison with Auto ARIMA showed that the manually tuned SARIMA Model 2 better captured seasonal fluctuations, especially during high-demand periods, such as holidays. Although Auto ARIMA offered efficiency, Model 2 consistently provided more accurate predictions, aligning with Glen Retailers’ need for precise seasonal planning.

### 6. Conclusions & Recommendations  
   The SARIMA Model 2 provides Glen Food & Beverage with actionable insights to prepare for seasonal demand, optimizing inventory and staffing during peak periods. Implementing SARIMA Model 2 will enhance Glen’s resource allocation, reduce costs associated with stockouts or overstock, and support growth and customer satisfaction.

---

## License
This project is for academic purposes and follows St. John’s University guidelines.

---

## Acknowledgments
Special thanks to **Professor Michael D. Herley** for his guidance and to **St. John’s University** for supporting this research initiative.

