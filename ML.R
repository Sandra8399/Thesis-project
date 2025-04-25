# Load required libraries
library(readxl)
library(writexl)
library(tidymodels)
library(glmnet)
library(caret)
library(randomForest)
library(xgboost)
library(dplyr)
library(ggplot2)
library(pROC)


#Logistic regression
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


# --- 1. BOXPLOT of Balanced Accuracy ---

all_performance_flat$combo <- paste0(
  "pen", all_performance_flat$penalty,
  "_mix", all_performance_flat$mixture
)

lr_boxplot <- ggplot(all_performance_flat, aes(x = combo, y = balanced_accuracy)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Balanced Accuracy by Hyperparameter (Logistic Regression)",
       x = "Penalty/Mixture Combo",
       y = "Balanced Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))


# --- 2. ROC Curve for Best Model on Fold 1 ---

# Get best combo
logreg_summary <- aggregate(balanced_accuracy ~ penalty + mixture, data = all_performance_flat, FUN = mean)
best_logreg <- logreg_summary[which.max(logreg_summary$balanced_accuracy), ]

cat("Best Logistic Regression Hyperparameters:\n")
print(best_logreg)

# Reload Fold 1
train <- read_excel("Adding_column_output/training_dataset_final_fold_1.xlsx")
test <- read_excel("Adding_column_output/testing_dataset_final_fold_1.xlsx")
train <- as.data.frame(train); test <- as.data.frame(test)
train$Sample <- NULL; test$Sample <- NULL
train$Node_status <- as.factor(train$Node_status); test$Node_status <- as.factor(test$Node_status)
colnames(train)[colnames(train) == "Node_status"] <- "Label"
colnames(test)[colnames(test) == "Node_status"] <- "Label"

# Fit model
best_log_model <- logistic_reg(
  penalty = best_logreg$penalty,
  mixture = best_logreg$mixture
) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(Label ~ ., data = train)

# Predict probabilities
probs <- predict(best_log_model, new_data = test, type = "prob")
roc_obj <- roc(as.numeric(test$Label) - 1, probs$.pred_1)

# Plot the ROC
png("Balanced accurycy and ROC results/lr_ROC.png", width = 800, height = 600, res = 150)
plot(roc_obj, main = paste0("ROC - Logistic Regression (AUC = ", round(auc(roc_obj), 3), ")"),
     col = "#0072B2", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()



############# Random forest ##################
# Helper to clean names
clean_names <- function(x) {
  x <- gsub("[^A-Za-z0-9*]", "", x)
  x <- gsub("\\*", "2", x)
  return(x)
}

# Hyperparameter grid
ntree     <- c(100, 500, 1000)
max_nodes <- c(10, 50, 100)
nodesize  <- c(1, 5, 10)
sampsize  <- c(10, 30, 50)

total_models_rf <- length(ntree) * length(max_nodes) * length(nodesize) * length(sampsize)

# Store performance from all folds
performance_all_folds_rf <- list()

for (fold in 1:5) {
  cat("Processing Fold", fold, "\n")
  
  # Load and clean data
  train_data_final <- read_excel(paste0("Adding_column_output/training_dataset_final_fold_", fold, ".xlsx"))
  test_data_final  <- read_excel(paste0("Adding_column_output/testing_dataset_final_fold_", fold, ".xlsx"))
  
  train_data_final <- as.data.frame(train_data_final)
  test_data_final  <- as.data.frame(test_data_final)
  
  # Remove sample name column
  train_data_final$Sample <- NULL
  test_data_final$Sample  <- NULL
  
  # Convert to factor
  train_data_final$Node_status <- as.factor(train_data_final$Node_status)
  test_data_final$Node_status  <- as.factor(test_data_final$Node_status)
  
  # Rename outcome column
  colnames(train_data_final)[colnames(train_data_final) == "Node_status"] <- "Label"
  colnames(test_data_final)[colnames(test_data_final) == "Node_status"] <- "Label"
  
  # Clean column and row names
  colnames(train_data_final) <- clean_names(colnames(train_data_final))
  rownames(train_data_final) <- clean_names(rownames(train_data_final))
  colnames(test_data_final)  <- clean_names(colnames(test_data_final))
  rownames(test_data_final)  <- clean_names(rownames(test_data_final))
  
  # Store performances
  performance_list_rf <- list()
  
  for (i in 1:length(ntree)) {
    for (j in 1:length(max_nodes)) {
      for (k in 1:length(nodesize)) {
        for (p in 1:length(sampsize)) {
          set.seed(123)
          
          model <- randomForest(Label ~ .,
                                ntree      = ntree[i],
                                max_nodes  = max_nodes[j],
                                nodesize   = nodesize[k],
                                sampsize   = sampsize[p],
                                data       = train_data_final)
          
          predictions <- predict(model, newdata = test_data_final)
          
          cm <- confusionMatrix(data = predictions, reference = test_data_final$Label)
          
          perf <- list(
            fold = fold,
            ntree = ntree[i],
            max_nodes = max_nodes[j],
            nodesize = nodesize[k],
            sampsize = sampsize[p],
            accuracy = cm$overall[["Accuracy"]],
            sensitivity = cm$byClass[["Sensitivity"]],
            specificity = cm$byClass[["Specificity"]],
            balanced_accuracy = cm$byClass[["Balanced Accuracy"]]
          )
          performance_list_rf[[length(performance_list_rf) + 1]] <- perf
        }
      }
    }
  }
  
  performance_all_folds_rf[[fold]] <- performance_list_rf
}

# Flatten results into a single data.frame
all_rf_performance_flat <- do.call(rbind, lapply(performance_all_folds_rf, function(fold_list) {
  do.call(rbind, lapply(fold_list, as.data.frame))
}))

# --- 1. BOXPLOT ---

# Add combo column for boxplot
all_rf_performance_flat$combo <- paste0(
  "ntree", all_rf_performance_flat$ntree,
  "_max", all_rf_performance_flat$max_nodes,
  "_node", all_rf_performance_flat$nodesize,
  "_samp", all_rf_performance_flat$sampsize
)

rf_boxplot <- ggplot(all_rf_performance_flat, aes(x = combo, y = balanced_accuracy)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Balanced Accuracy by Hyperparameter (Random Forest)",
       x = "RF Hyperparameter Combo",
       y = "Balanced Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

# --- 2. ROC Curve ---

# Get best hyperparameters based on mean balanced accuracy
rf_summary <- all_rf_performance_flat %>%
  group_by(ntree, max_nodes, nodesize, sampsize) %>%
  summarise(mean_bal_acc = mean(balanced_accuracy), .groups = "drop") %>%
  arrange(desc(mean_bal_acc))

best_rf <- rf_summary[1, ]
cat("Best RF Hyperparameters:\n")
print(best_rf)

# Reload Fold 1 for ROC visualization
train <- read_excel("Adding_column_output/training_dataset_final_fold_1.xlsx")
test <- read_excel("Adding_column_output/testing_dataset_final_fold_1.xlsx")
train <- as.data.frame(train); test <- as.data.frame(test)
train$Sample <- NULL; test$Sample <- NULL
train$Node_status <- as.factor(train$Node_status); test$Node_status <- as.factor(test$Node_status)
colnames(train)[colnames(train) == "Node_status"] <- "Label"
colnames(test)[colnames(test) == "Node_status"] <- "Label"
colnames(train) <- clean_names(colnames(train))
colnames(test)  <- clean_names(colnames(test))

# Train model with best params
set.seed(123)
rf_best <- randomForest(Label ~ ., data = train,
                        ntree = best_rf$ntree,
                        max_nodes = best_rf$max_nodes,
                        nodesize = best_rf$nodesize,
                        sampsize = best_rf$sampsize)

# Predict probs and plot ROC
rf_probs <- predict(rf_best, newdata = test, type = "prob")[,2]
labels <- as.numeric(as.factor(test$Label)) - 1

#print the ROC
png("Balanced accurycy and ROC results/rf_ROC.png", width = 800, height = 600, res = 150)
roc_obj_rf <- roc(labels, rf_probs)
plot(roc_obj_rf, main = paste0("ROC - Random Forest (AUC = ", round(auc(roc_obj_rf), 3), ")"),
     col = "#D55E00", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()



####################### XGBoost ####################
# Define hyperparameter grid
nrounds    <- c(50, 100, 200)
max_depth  <- c(3, 6, 10)
eta        <- c(0.1, 0.3, 0.6)
reg_alpha  <- c(0, 0.5, 1)
reg_gamma  <- c(0, 0.5, 1)
subsample  <- c(0.5, 1)

total_models_xgb <- length(nrounds) * length(max_depth) * length(eta) *
  length(reg_alpha) * length(reg_gamma) * length(subsample)

# Helper function
clean_names <- function(x) {
  x <- gsub("[^A-Za-z0-9*]", "", x)
  x <- gsub("\\*", "2", x)
  return(x)
}

# Store results across folds
performance_all_folds_xgb <- list()

for (fold in 1:5) {
  cat("Processing Fold", fold, "\n")
  
  # Load data
  train_data_final <- read_excel(paste0("Adding_column_output/training_dataset_final_fold_", fold, ".xlsx"))
  test_data_final  <- read_excel(paste0("Adding_column_output/testing_dataset_final_fold_", fold, ".xlsx"))
  
  # Clean and prepare
  train_data_final <- as.data.frame(train_data_final)
  test_data_final  <- as.data.frame(test_data_final)
  
  train_data_final$Sample <- NULL
  test_data_final$Sample  <- NULL
  
  train_data_final$Node_status <- as.factor(train_data_final$Node_status)
  test_data_final$Node_status  <- as.factor(test_data_final$Node_status)
  
  colnames(train_data_final)[colnames(train_data_final) == "Node_status"] <- "Label"
  colnames(test_data_final)[colnames(test_data_final) == "Node_status"] <- "Label"
  
  colnames(train_data_final) <- clean_names(colnames(train_data_final))
  colnames(test_data_final)  <- clean_names(colnames(test_data_final))
  
  # Extract label and features
  train_labels <- as.numeric(as.factor(train_data_final$Label)) - 1
  test_labels  <- as.numeric(as.factor(test_data_final$Label)) - 1
  
  train_matrix <- as.matrix(train_data_final[, setdiff(names(train_data_final), "Label")])
  test_matrix  <- as.matrix(test_data_final[, setdiff(names(test_data_final), "Label")])
  
  dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
  dtest  <- xgb.DMatrix(data = test_matrix, label = test_labels)
  
  # Store performance for fold
  performance_list_xgb <- list()
  
  for (i in 1:length(nrounds)) {
    for (j in 1:length(max_depth)) {
      for (k in 1:length(eta)) {
        for (p in 1:length(reg_alpha)) {
          for (m in 1:length(reg_gamma)) {
            for (n in 1:length(subsample)) {
              set.seed(123)
              
              xgb_model <- xgboost(
                data = dtrain,
                objective = "binary:logistic",
                eval_metric = "auc",
                nrounds = nrounds[i],
                max_depth = max_depth[j],
                eta = eta[k],
                reg_alpha = reg_alpha[p],
                reg_gamma = reg_gamma[m],
                subsample = subsample[n],
                verbose = 0
              )
              
              xgb_pred_prob <- predict(xgb_model, dtest)
              pred <- as.numeric(xgb_pred_prob > 0.5)
              
              cm <- confusionMatrix(
                data = factor(pred, levels = c(0, 1)),
                reference = factor(test_labels, levels = c(0, 1))
              )
              
              perf <- list(
                fold = fold,
                nrounds = nrounds[i],
                max_depth = max_depth[j],
                eta = eta[k],
                reg_alpha = reg_alpha[p],
                reg_gamma = reg_gamma[m],
                subsample = subsample[n],
                accuracy = cm$overall[["Accuracy"]],
                sensitivity = cm$byClass[["Sensitivity"]],
                specificity = cm$byClass[["Specificity"]],
                balanced_accuracy = cm$byClass[["Balanced Accuracy"]]
              )
              
              performance_list_xgb[[length(performance_list_xgb) + 1]] <- perf
            }
          }
        }
      }
    }
  }
  
  performance_all_folds_xgb[[fold]] <- performance_list_xgb
}

# Flatten and summarize
all_xgb_performance_flat <- do.call(rbind, lapply(performance_all_folds_xgb, function(fold_list) {
  do.call(rbind, lapply(fold_list, as.data.frame))
}))

# --- 1. BOXPLOT ---

# Add combo label
all_xgb_performance_flat$combo <- paste0(
  "n", all_xgb_performance_flat$nrounds,
  "_d", all_xgb_performance_flat$max_depth,
  "_eta", all_xgb_performance_flat$eta,
  "_a", all_xgb_performance_flat$reg_alpha,
  "_g", all_xgb_performance_flat$reg_gamma,
  "_s", all_xgb_performance_flat$subsample
)

xgb_boxplot <- ggplot(all_xgb_performance_flat, aes(x = combo, y = balanced_accuracy)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Balanced Accuracy by Hyperparameter (XGBoost)",
       x = "XGBoost Hyperparameter Combo",
       y = "Balanced Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

# --- 2. BEST PARAMS + ROC CURVE ---

# Summary of top performance by average balanced accuracy
xgb_summary <- all_xgb_performance_flat %>%
  group_by(nrounds, max_depth, eta, reg_alpha, reg_gamma, subsample) %>%
  summarise(mean_bal_acc = mean(balanced_accuracy), .groups = "drop") %>%
  arrange(desc(mean_bal_acc))

# Print best hyperparameters
cat("Best XGBoost hyperparameter combination by balanced accuracy:\n")
print(xgb_summary[1, ])

# Reload Fold 1 data for ROC curve
train <- read_excel("Adding_column_output/training_dataset_final_fold_1.xlsx")
test <- read_excel("Adding_column_output/testing_dataset_final_fold_1.xlsx")
train <- as.data.frame(train); test <- as.data.frame(test)
train$Sample <- NULL; test$Sample <- NULL
train$Node_status <- as.factor(train$Node_status); test$Node_status <- as.factor(test$Node_status)
colnames(train)[colnames(train) == "Node_status"] <- "Label"
colnames(test)[colnames(test) == "Node_status"] <- "Label"
colnames(train) <- clean_names(colnames(train))
colnames(test)  <- clean_names(colnames(test))

# Extract matrix and label
train_labels <- as.numeric(as.factor(train$Label)) - 1
test_labels  <- as.numeric(as.factor(test$Label)) - 1
train_matrix <- as.matrix(train[, setdiff(names(train), "Label")])
test_matrix  <- as.matrix(test[, setdiff(names(test), "Label")])

dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest  <- xgb.DMatrix(data = test_matrix, label = test_labels)

# Fit best XGB model
best_params <- xgb_summary[1, ]
set.seed(123)
xgb_best <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = best_params$nrounds,
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  reg_alpha = best_params$reg_alpha,
  reg_gamma = best_params$reg_gamma,
  subsample = best_params$subsample,
  verbose = 0
)

# Predict probs and plot ROC
png("Balanced accurycy and ROC results/xgb_ROC.png", width = 800, height = 600, res = 150)
xgb_probs <- predict(xgb_best, dtest)
roc_obj_xgb <- roc(test_labels, xgb_probs)
plot(roc_obj_xgb, main = paste0("ROC - XGBoost (AUC = ", round(auc(roc_obj_xgb), 3), ")"),
     col = "#0072B2", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()




#print boxplots
png("Balanced accurycy and ROC results/lr_boxplot.png", width = 800, height = 600, res = 150)
print(lr_boxplot)  # Print the plot
dev.off()

# RF
png("Balanced accurycy and ROC results/rf_boxplot.png", width = 800, height = 600, res = 150)
print(rf_boxplot)  # Print the plot
dev.off()

# XGBoost
png("Balanced accurycy and ROC results/xgb_boxplot.png", width = 800, height = 600, res = 150)
print(xgb_boxplot)  # Print the plot
dev.off()
