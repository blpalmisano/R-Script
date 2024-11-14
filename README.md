# ✈️ Case Study #1: Predicting Flight Delays using Multiple Linear Regression

## Overview
This repository contains materials for a case study on predicting flight arrival delays in the airline industry. Using multiple linear regression in R-Script, the project models and forecasts delays based on key operational factors. By identifying influential variables, the analysis provides insights into optimizing flight schedules and improving reliability. This work highlights the value of predictive analytics in tackling one of the airline industry's biggest challenges: minimizing delays and enhancing customer satisfaction.

### Authors
- **Brianna L. Palmisano**
- **Julianna LoMonte**
- **Ava C. Rice**

### Course
- **Course Title**: Seminar in Business Analytics, Business Analytics & Information Systems Department
- **Professor**: Dr. Michael D. Herley, D.B.A.
- **Institution**: St. John’s University, The Peter J. Tobin College of Business
- **Semester**: Fall 2024

---

## Repository Contents
- **PowerPoint Presentation**  
  - `Case Study 1 - Part 1.pptx`: Covers the introduction, airline industry overview, and the importance of delay prediction.
  - `Case Study 1 - Part 2.pptx`: Discusses the multiple linear regression model, variable selection, evaluation of the model, and final conclusions.

- **R Script**  
  - `CaseStudy1.Group1.R`: Contains data preprocessing, model building, assumption testing, and evaluation for the multiple linear regression analysis.

- **Dataset**  
  - `Chapter_06_flight_delay.csv`: Contains airline operational data used to build and test the model, with variables such as Airport Distance, Number of Flights, Weather, Support Crew, and Late Arrival.

---

## Project Structure

### 1. Introduction & Industry Overview  
   This section provides background on the global airline industry, highlighting its economic impact and operational challenges. Delays in flight arrivals cost airlines millions in operational expenses and negatively impact customer satisfaction. The business problem focuses on identifying predictors that significantly influence delays, aiming to develop a model that aids in proactive scheduling adjustments.

### 2. Application of Analytics in the Airline Industry  
   Predictive analytics allows airlines to analyze vast data sets, including flight operations, weather, and crew availability. The model identifies trends in variables that contribute to delays, helping airlines mitigate risks associated with scheduling and improve operational efficiency in areas such as:
   - **Personalized Offers & Passenger Experience**
   - **Safety Enhancements**
   - **Fraud Detection**
   - **Delay Prediction**

### 3. Model Development  
   The multiple linear regression model was built using selected variables that showed strong relationships with delays during exploratory data analysis. Key variables include:
   - **Number of Flights**: Higher frequencies increase the risk of delays.
   - **Airport Distance**: Longer routes may require additional buffer times.
   - **Support Crew Availability**: Sufficient staffing can reduce turnaround delays.
   - **Weather Conditions**: Unpredictable weather impacts flight schedules.
   - **Late Arrival of Previous Flights**: Prior delays often lead to cascading effects.

   Assumptions of the regression model were tested to ensure validity, including checks for linearity, normality, and homoscedasticity.

### 4. Evaluation  
   Model performance was evaluated based on R-squared and adjusted R-squared values, residual plots, and other diagnostics to confirm the model's accuracy. Residual analysis helped detect any outliers or influential observations. The high explanatory power (82% of variance in delays) indicates that the model provides a reliable framework for understanding delay factors.

### 5. Conclusion  
   The analysis concludes with actionable recommendations for airlines to manage delays by optimizing key operational variables. Suggested improvements include enhancing baggage handling, optimizing crew schedules, and integrating real-time weather data for better forecasting. The model demonstrates that data-driven decision-making can significantly enhance airline scheduling efficiency and customer satisfaction.

---

## License
This project is for academic purposes and follows St. John’s University guidelines.

---

## Acknowledgments
Special thanks to **Professor Michael D. Herley** for his guidance and to **St. John’s University** for supporting this research initiative.
