# 🧠 Credit Risk Modeling – Robust Linear and Logistic Regression

This project presents a robust statistical analysis on a real-world credit risk dataset, with the goal of predicting loan amounts and loan default probability based on borrower and loan characteristics.

## 📊 Objectives

- Build and validate a **robust linear regression model** to explain loan amount (`loan_amnt`)
- Develop a **logistic regression model** to classify loan default status (`loan_status`)
- Apply rigorous data preprocessing, transformation, and diagnostics to ensure model quality

## 📁 Dataset

- **Source**: Kaggle – Credit Risk Dataset
- **Observations**: 32,581
- **Variables**: 12 features (continuous and categorical)
- **Targets**:
  - `loan_amnt` (regression)
  - `loan_status` (binary classification)

## 🔧 Key Techniques

- Missing data imputation using **MICE**
- Variable transformations:
  - Box-Cox (for `loan_amnt`)
  - Logarithmic and polynomial (for predictors)
- Multicollinearity check via **VIF** and **Tolerance**
- Outlier detection and treatment (Cook’s distance, leverage)
- Heteroskedasticity correction:
  - White’s standard errors
  - Weighted Least Squares (**WLS**)
- Stepwise **BIC model selection**
- Bootstrap validation with **1999 replicates**
- Logistic regression analysis with odds ratio interpretation
- Evaluation metrics: **Adjusted R²**, **AIC**, **Confusion Matrix**, **Accuracy**

## 📈 Results

- Final linear model:
  - Adjusted R² = **0.9757**
  - AIC = **112,330.1**
- Final logistic model:
  - Accuracy = **86.7%**
  - False positive rate = 3.6%
  - False negative rate = 9.7%

## 🛠️ Tools & Libraries

- **R**, **RStudio**
- `car`, `mice`, `MASS`, `ggplot2`, `boot`, `factorMerger`, and base R
- Diagnostic plotting and visualization

## 📌 Notes

This project was developed as part of a Data Mining course during the academic year 2024/2025, aiming to apply advanced statistical methodologies to real-world financial data.
