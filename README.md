# Comparative Usage of Machine Learning

This project explores the comparative performance of three machine learning (ML) models, Logistic Regression (LR), Random Forest (RF), and XGBoost, for their potential application as diagnostic tools for endometrial cancer (EC).

## Project Overview

The dataset underwent pre-processing including the removal of NA values, quantile normalization, and z-score standardization. Before wuamtile normalization, the dataset was split into training and testing sets using a 5-fold nested cross validation. Each ML model was trained on the training data and evaluated using the ROC curve and balanced accuracy metrics. The goal was to determine which model performed best in classifying EC cases.

## Repository Contents

- **Book.xlsx** — Excel file, containing the original dataset
- **Removing NA and NaN values.R** — R file that cleans the dataset by removing NA and NaN values
- **cleaned_dataset.xlsx** — Dataset after the NA/NaN removal
- **Quantile normalization.R** — R file that performs nested cross validation and quantile normalization
- **Quantile_normalization_output** — Output folder containing results from quantile normalization
- **Standardization.R** — R file that performs z-score standardization
- **Scaling_output** — Output folder containing files resulting from z-score scaling
- **Add node status.R** — R file that performs that adds lymph node status of each sample
- **Adding_column_output** — Output folder after adding lymph node status column to the dataset
- **ML.R** — Main script implementing the machine learning models (LR, RF, XGBoost)

## Technologies Used

- R programming language
- Machine Learning algorithms: Logistic Regression, Random Forest, XGBoost

---

