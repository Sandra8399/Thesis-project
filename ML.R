library(readxl)
library(writexl)

# Load datasets
train_data <- read_excel("training_scaled_dataset.xlsx")
test_data <- read_excel("testing_scaled_dataset.xlsx")

# Identify sample names (excluding 'Gene_symbol')
sample_status <- colnames(train_data)[-1]  

# Identify whether samples are lymph node negative or positive
status_binary <- ifelse(grepl("-IIIC$", sample_status), 0, 1)

# Convert to a dataframe & transpose (Make sure to keep the labels in the correct format)
sample_labels <- data.frame(Label = status_binary)

# Transpose train_data so that samples are rows and genes are columns
train_data_t <- as.data.frame(t(train_data[-1]))  # Exclude the 'Gene_symbol' column before transposing
colnames(train_data_t) <- train_data$Gene_symbol  # Rename columns using Gene_symbol

# Reset row names to avoid "V1, V2, ..." issue (using the sample names as row names)
rownames(train_data_t) <- colnames(train_data)[-1]  # Use the sample names as row names

# Add the Label column (Ensure the number of rows matches between train_data_t and sample_labels)
train_data_final <- cbind(train_data_t, Label = sample_labels$Label)  # Add Label column

train_data_final$Label <- as.factor(train_data_final$Label)

# Repeat for testing datalibrary(readxl)

# Identify sample names (excluding 'Gene_symbol')
sample_status <- colnames(test_data)[-1]  

# Identify whether samples are lymph node negative or positive
status_binary <- ifelse(grepl("-IIIC$", sample_status), 0, 1)

# Convert to a dataframe & transpose (Make sure to keep the labels in the correct format)
sample_labels <- data.frame(Label = status_binary)

# Transpose test_data so that samples are rows and genes are columns
test_data_t <- as.data.frame(t(test_data[-1]))  # Exclude the 'Gene_symbol' column before transposing
colnames(test_data_t) <- test_data$Gene_symbol  # Rename columns using Gene_symbol

# Reset row names to avoid "V1, V2, ..." issue (using the sample names as row names)
rownames(test_data_t) <- colnames(test_data)[-1]  # Use the sample names as row names

# Add the Label column (Ensure the number of rows matches between test_data_t and sample_labels)
test_data_final <- cbind(test_data_t, Label = sample_labels$Label)  # Add Label column


#Logistic regression
library(tidymodels)
library(glmnet)

logistic_regression_train <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(Label ~ ., data = train_data_final)

# Print model summary
summary(logistic_regression_train$fit)
dim(train_data_final)
dim(test_data_final)
#package, library
lr_prediction_test <- predict(logistic_regression_train, new_data = test_data_final)
summary(lr_prediction_test)

#can run confusion matrix
table(test_data_final$Label)
#can tune hyperparameters

#Random forest
#install.packages("randomForest")
library(randomForest)
set.seed(123)
random_forest_train <- randomForest(Label ~ ., #what is predicted, what do I put?
                                    data = train_data_final)

rf_prediction_test <- predict(random_forest_train, newdata = test)

#XGBoost
