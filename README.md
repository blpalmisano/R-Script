# 🏦 Case Study #2: Predicting Bank-Loan Defaults Using Logistic Regression

## Overview
This repository contains materials for a case study on predicting loan defaults in the banking industry. Using logistic regression in R, this analysis identifies key financial and demographic predictors that influence the likelihood of default. By focusing on high-impact variables, the model supports credit risk management and helps banks make informed, data-driven lending decisions. This project was completed as part of the **Seminar in Business Analytics** course at **St. John’s University, The Peter J. Tobin College of Business**.

### Authors
- **Brianna L. Palmisano**
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
  - `Case Study 2.pptx`: Slides covering the industry background, model development, evaluation, and conclusions.

- **R Script**  
  - `CaseStudy.2.R`: R script containing data preprocessing, logistic regression modeling, assumption checks, and model validation.

- **Dataset**  
  - `loan_default.csv`: Dataset of 1,000 loan records with financial and demographic information used for predicting loan default risk.

---

## Project Structure

### 1. Introduction & Industry Overview  
   This section provides context on the significance of credit risk management in the banking sector, focusing on loan defaults as a major financial risk. The business objective is to identify factors contributing to loan defaults to aid banks in making data-driven decisions that support stability and responsible lending.

### 2. Exploratory Data Analysis  
   Initial data exploration revealed trends among key variables:
   - **Checking Amount**: A wide range indicating financial diversity among borrowers.
   - **Credit Score**: Scores varied broadly, suggesting diverse creditworthiness.
   - **Savings and Employment Duration**: Indicators of financial stability linked to default risk.

### 3. Model Development  
   Logistic regression was used to model the probability of loan defaults based on predictors with high explanatory power, including:
   - **Checking Account Balance**: Higher balances reduce the odds of default.
   - **Loan Term**: Longer terms are associated with increased default risk.
   - **Credit Score**: Higher scores correlate with lower default risk.
   - **Employment Status**: Stability in employment is linked to reduced default likelihood.

   Predictor variables were refined based on linearity, correlation, and statistical significance, resulting in a final model that optimally balances predictive accuracy and interpretability.

### 4. Model Evaluation  
   Evaluation metrics include:
   - **Wald Test**: Confirms the significance of individual predictors in the model.
   - **Hosmer-Lemeshow Test**: Verifies model fit with a p-value > 0.05, indicating good fit.
   - **AUC and ROC Curve**: An AUC of 0.986 signifies strong model performance, accurately distinguishing between default and non-default cases.

### 5. Model Validation  
   The model was validated using a 70/30 train-test split to ensure reliability on unseen data. With an accuracy of 92.3% and a low misclassification error, the model effectively predicts loan defaults, balancing precision and recall to minimize false positives and false negatives.

### 6. Conclusion  
   The final model offers a reliable tool for assessing credit risk, emphasizing critical factors like account balance, loan term, credit score, and employment status. These insights support lenders in mitigating financial risk while upholding responsible lending standards. Future work could enhance the model with additional predictors or explore machine learning techniques for even greater predictive accuracy.

---

## License
This project is for academic purposes and follows St. John’s University guidelines.

---

## Acknowledgments
Special thanks to **Professor Michael D. Herley** for his guidance and to **St. John’s University** for supporting this research initiative.

