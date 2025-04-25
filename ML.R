# Load required libraries
library(readxl)
library(writexl)
library(tidymodels)
library(glmnet)
library(caret)

# Hyperparameter grids
penalty <- c(0, 0.5, 1)
mixture <- c(0, 0.5, 1)
total_models_lr <- length(penalty) * length(mixture)

# Prepare results list
performance_all_folds <- list()

for (fold in 1:5) {
  cat("Running Fold", fold, "\n")
  
  # === Load data ===
  train_data_final <- read_excel(paste0("Adding_column_output/training_dataset_final_fold_", fold, ".xlsx"))
  test_data_final  <- read_excel(paste0("Adding_column_output/testing_dataset_final_fold_", fold, ".xlsx"))
  
  # Convert to data.frame
  train_data_final <- as.data.frame(train_data_final)
  test_data_final  <- as.data.frame(test_data_final)
  
  # Drop Sample column
  train_data_final$Sample <- NULL
  test_data_final$Sample <- NULL
  
  # Ensure factor type for outcome
  train_data_final$Node_status <- as.factor(train_data_final$Node_status)
  test_data_final$Node_status  <- as.factor(test_data_final$Node_status)
  
  # Rename for modeling (optional, but matches your original code)
  colnames(train_data_final)[colnames(train_data_final) == "Node_status"] <- "Label"
  colnames(test_data_final)[colnames(test_data_final) == "Node_status"] <- "Label"
  
  # Performance list for current fold
  performance_list_log_reg <- list()
  
  for (i in 1:length(penalty)) {
    for (j in 1:length(mixture)) {
      # Fit model
      model <- logistic_reg(penalty = penalty[i], mixture = mixture[j]) %>%
        set_engine("glmnet") %>%
        set_mode("classification") %>%
        fit(Label ~ ., data = train_data_final)
      
      # Predict
      predictions <- predict(model, new_data = test_data_final)
      
      # Confusion matrix
      cm <- confusionMatrix(data = predictions$.pred_class, reference = test_data_final$Label)
      
      # Save performance
      performance <- list(
        fold = fold,
        penalty = penalty[i],
        mixture = mixture[j],
        accuracy = cm$overall[["Accuracy"]],
        sensitivity = cm$byClass[["Sensitivity"]],
        specificity = cm$byClass[["Specificity"]],
        balanced_accuracy = cm$byClass[["Balanced Accuracy"]]
      )
      performance_list_log_reg[[length(performance_list_log_reg) + 1]] <- performance
    }
  }
  
  # Store for this fold
  performance_all_folds[[fold]] <- performance_list_log_reg
}

# === Extract and summarize all performances ===
all_performance_flat <- do.call(rbind, lapply(performance_all_folds, function(fold_perf) {
  do.call(rbind, lapply(fold_perf, as.data.frame))
}))

# View results
print(all_performance_flat)



# Import necessary libraries
library(readxl)
library(writexl)
library(tidymodels)
library(glmnet)
library(caret)

# Load datasets
train_data_final <- read_excel("training_dataset_final.xlsx")
test_data_final <- read_excel("testing_dataset_final.xlsx")

# Make sure both datasets are dataframe
train_data_final <- as.data.frame(train_data_final)
test_data_final <- as.data.frame(test_data_final)

# Make sure Node_status is a factor
train_data_final$Node_status <- as.factor(train_data_final$Node_status)
test_data_final$Node_status <- as.factor(test_data_final$Node_status)

# Default
# Logistic regression

logistic_regression_train <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(Node_status ~ ., data = train_data_final)

# Print model summary
summary(logistic_regression_train$fit)
dim(train_data_final)
dim(test_data_final)
#package, library
lr_prediction_test <- predict(logistic_regression_train, new_data = test_data_final)


# Tuning Hyperparameters

# 1. Penalty: Regularization
# 2. Mixture: Proportion of Lasso Penalty
# mixture = 1 (pure lasso model) (l1 regularization)
# mixture = 0 ridge regression model (l2 regularization)
# mixture = [0-1] elastic net model (combo lasso and ridge)


#### Penalty
penalty<-c(0,0.5,1)
penalty

#### Mixture
mixture<-c(0,0.5,1)
mixture

total_models_lr<-length(penalty) * length(mixture)
total_models_lr
# data frame

df<-data.frame(penalty=rep(penalty,each=3),
               mixture=rep(mixture,times=3))
df

plot(df$mixture,df$penalty)

performance_list_log_reg<-list()
for(a in 1:length(folds)){
  for(i in 1:length(penalty)){
    for(j in 1:length(mixture)){
      model <- logistic_reg(penalty = penalty[i], mixture = mixture[j]) %>%
        set_engine("glmnet") %>%
        set_mode("classification") %>%
        fit(Label ~ ., data = train_data_final)
      
      lr_prediction_test <- predict(model, new_data = test_data_final)
      
      cm<-confusionMatrix(data=lr_prediction_test$.pred_class, reference = test_data_final$Label)
      
      performance<-list(penalty=penalty[i],
                        mixture=mixture[j],
                        accuracy=cm$overall[["Accuracy"]],
                        sensitivity=cm$byClass[["Sensitivity"]],
                        specificity=cm$byClass[["Specificity"]],
                        balanced_accuracy=cm$byClass[["Balanced Accuracy"]])
      performance_list_log_reg[[length(performance_list_log_reg)+1]]<-performance
    }
  }
}

performance_list_log_reg


performance_list_log_reg[[1]]$balanced_accuracy

balanced_accuracy_lr<-sapply(performance_list_log_reg,function(x){
  bal_accuracy<-x$balanced_accuracy
  return(bal_accuracy)
}
)

results_lr<-data.frame(balanced_accuracy_lr,
                       model=factor(seq(from=1,to=total_models_lr)))
results_lr

plot(y=results_lr$balanced_accuracy_lr,x=results_lr$model,ylab="Balanced Accuracy",xlab="Model number")












# Compare predicted results to real data
summary(lr_prediction_test)
table(test_data_final$Node_status)

#Confusion matrix
confusionMatrix(data=lr_prediction_test$.pred_class, reference = test_data_final$Node_status)


# Remove all "-" characters and replace all "*" characters, so that RF can work
clean_names <- function(x) {
  x <- gsub("[^A-Za-z0-9*]", "", x)  # Remove all special characters except *
  x <- gsub("\\*", "2", x)           # Replace * with 2
  return(x)
}

# Columns & rows replacement
colnames(train_data_final) <- clean_names(colnames(train_data_final))
rownames(train_data_final) <- clean_names(rownames(train_data_final))

# Repeat for testing
colnames(test_data_final) <- clean_names(colnames(test_data_final))
rownames(test_data_final) <- clean_names(rownames(test_data_final))


#remove all - & * (dupes) from the genes, samples etc (special characters)
#Random forest
library(randomForest)
set.seed(123)
random_forest_train <- randomForest(Node_status ~ .,
                                    data = train_data_final)
summary(random_forest_train)

rf_prediction_test <- predict(random_forest_train, newdata = test_data_final)

summary(rf_prediction_test)
table(test_data_final$LabeNode_statusl)

#Confusion matrix
confusionMatrix(data=rf_prediction_test, reference = test_data_final$Node_status)

#XGBoost
library(xgboost)

# Prepare for XGBoost
# Extract labels
train_labels <- as.numeric(as.factor(train_data_final$Node_status)) - 1
test_labels  <- as.numeric(as.factor(test_data_final$Node_status)) - 1

# Extract features (remove the label column)
train_matrix <- as.matrix(train_data_final[, setdiff(names(train_data_final), "Node_status")])
test_matrix  <- as.matrix(test_data_final[, setdiff(names(test_data_final), "Node_status")])

# Create DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest  <- xgb.DMatrix(data = test_matrix, label = test_labels)

getinfo(dtrain, "Node_status")  # returns the labels
dim(dtrain)               # returns dimensions


# Run XGBoost
xgb_model <- xgboost(
  data = dtrain,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "logloss"
)

# Predict probabilities on the test set
xgb_prediction_test <- predict(xgb_model, dtest)

prediction <- as.numeric(xgb_prediction_test > 0.5)
print(head(prediction))

summary(prediction)
table(test_data_final$Node_status)

#Confusion matrix
# Node_status as 0/1
labels <- as.numeric(as.factor(test_data_final$Node_status)) - 1

# Confusion matrix
confusionMatrix(
  data = factor(prediction, levels = c(0, 1)),
  reference = factor(labels, levels = c(0, 1))
)
