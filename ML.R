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

png("Balanced accurycy and ROC results/lr_boxplot.png", width = 800, height = 600, res = 150)

ggplot(all_performance_flat, aes(x = combo, y = balanced_accuracy)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Balanced Accuracy by Hyperparameter (Logistic Regression)",
       x = "Penalty/Mixture Combo",
       y = "Balanced Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
dev.off()

# --- 2. ROC Curve for Best Model ---

# Get best hyperparameters
logreg_summary <- aggregate(balanced_accuracy ~ penalty + mixture, data = all_performance_flat, FUN = mean)
best_logreg <- logreg_summary[which.max(logreg_summary$balanced_accuracy), ]

cat("Best Logistic Regression Hyperparameters:\n")
print(best_logreg)

# Initialize list to store ROC objects
roc_list_lr <- list()

# Loop through all 5 folds
for (i in 1:5) {
  # Load training and test data
  train <- read_excel(paste0("Adding_column_output/training_dataset_final_fold_", i, ".xlsx"))
  test <- read_excel(paste0("Adding_column_output/testing_dataset_final_fold_", i, ".xlsx"))
  
  # Clean and prep data
  train <- as.data.frame(train); test <- as.data.frame(test)
  train$Sample <- NULL; test$Sample <- NULL
  train$Node_status <- as.factor(train$Node_status); test$Node_status <- as.factor(test$Node_status)
  colnames(train)[colnames(train) == "Node_status"] <- "Label"
  colnames(test)[colnames(test) == "Node_status"] <- "Label"
  
  # Fit model using best hyperparameters
  model <- logistic_reg(
    penalty = best_logreg$penalty,
    mixture = best_logreg$mixture
  ) %>%
    set_engine("glmnet") %>%
    set_mode("classification") %>%
    fit(Label ~ ., data = train)
  
  # Predict probabilities
  probs <- predict(model, new_data = test, type = "prob")
  
  # Compute ROC
  roc_obj <- roc(as.numeric(test$Label) - 1, probs$.pred_1)
  roc_list_lr[[i]] <- roc_obj
}

# Plot all 5 ROC curves
png("Balanced accurycy and ROC results/lr_ROC_all_folds.png", width = 800, height = 600, res = 150)
plot(roc_list_lr[[1]], col=1, lwd=2, main="ROC - Logistic Regression (All 5 Folds)")
for (i in 2:5) {
  plot(roc_list_lr[[i]], add=TRUE, col=i, lwd=2)
}
abline(a=0, b=1, lty=2, col="gray")
legend("bottomright", legend=paste("Fold", 1:5), col=1:5, lwd=2)
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

# --- 1. BOXPLOT (Top 10 Combos by Balanced Accuracy) ---

# Create combo column for boxplot labels
all_rf_performance_flat$combo <- paste0(
  "ntree", all_rf_performance_flat$ntree,
  "_max", all_rf_performance_flat$max_nodes,
  "_node", all_rf_performance_flat$nodesize,
  "_samp", all_rf_performance_flat$sampsize
)

# Get top 10 combos based on mean balanced accuracy
top_combos <- all_rf_performance_flat %>%
  group_by(combo) %>%
  summarise(mean_bal_acc = mean(balanced_accuracy, na.rm = TRUE)) %>%
  arrange(desc(mean_bal_acc)) %>%
  slice_head(n = 10) %>%
  pull(combo)

# Filter data to include only top 10 combos
top_data <- filter(all_rf_performance_flat, combo %in% top_combos)

# (Optional) Create output folder
dir.create("rf_boxplots", showWarnings = FALSE)

# Plot
png("rf_boxplots/boxplot_top10.png", width = 800, height = 600, res = 150)
ggplot(top_data, aes(x = combo, y = balanced_accuracy)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Top 10 RF Combos by Balanced Accuracy",
       x = "RF Hyperparameter Combo",
       y = "Balanced Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

dev.off()


# --- 2. BOXPLOT All---

# Create combo column for boxplot labels
all_rf_performance_flat$combo <- paste0(
  "ntree", all_rf_performance_flat$ntree,
  "_max", all_rf_performance_flat$max_nodes,
  "_node", all_rf_performance_flat$nodesize,
  "_samp", all_rf_performance_flat$sampsize
)

# Get unique combos and split into chunks of 20
combos <- unique(all_rf_performance_flat$combo)
combo_chunks <- split(combos, ceiling(seq_along(combos) / 20))

# (Optional) Create output folder to save plots
dir.create("rf_boxplots", showWarnings = FALSE)

# Loop through chunks and plot
for (i in seq_along(combo_chunks)) {
  chunk <- combo_chunks[[i]]
  subset_data <- filter(all_rf_performance_flat, combo %in% chunk)
  
  p <- ggplot(subset_data, aes(x = combo, y = balanced_accuracy)) +
    geom_boxplot(fill = "orange", color = "black") +
    labs(title = paste("Balanced Accuracy - Chunk", i),
         x = "RF Hyperparameter Combo",
         y = "Balanced Accuracy") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
  
  print(p)  # Show plot in viewer
  
  # Save plot to the rf_boxplots folder
  ggsave(
    filename = paste0("rf_boxplots/boxplot_chunk_", i, ".png"),
    plot = p, width = 10, height = 6
  )
}


# --- 3. ROC Curve ---
# Get best hyperparameters based on mean balanced accuracy
rf_summary <- all_rf_performance_flat %>%
  group_by(ntree, max_nodes, nodesize, sampsize) %>%
  summarise(mean_bal_acc = mean(balanced_accuracy), .groups = "drop") %>%
  arrange(desc(mean_bal_acc))

best_rf <- rf_summary[1, ]
cat("Best RF Hyperparameters:\n")
print(best_rf)

# Initialize list to store ROC objects
roc_list_rf <- list()

# Loop through all 5 folds
for (i in 1:5) {
  # Reload Fold i
  train <- read_excel(paste0("Adding_column_output/training_dataset_final_fold_", i, ".xlsx"))
  test <- read_excel(paste0("Adding_column_output/testing_dataset_final_fold_", i, ".xlsx"))
  train <- as.data.frame(train); test <- as.data.frame(test)
  train$Sample <- NULL; test$Sample <- NULL
  train$Node_status <- as.factor(train$Node_status); test$Node_status <- as.factor(test$Node_status)
  colnames(train)[colnames(train) == "Node_status"] <- "Label"
  colnames(test)[colnames(test) == "Node_status"] <- "Label"
  colnames(train) <- clean_names(colnames(train))
  colnames(test)  <- clean_names(colnames(test))
  
  # Train model with best hyperparameters
  set.seed(123)
  rf_best <- randomForest(Label ~ ., data = train,
                          ntree = best_rf$ntree[[1]],
                          max_nodes = best_rf$max_nodes[[1]],
                          nodesize = best_rf$nodesize[[1]],
                          sampsize = best_rf$sampsize[[1]])
  
  # Predict probabilities
  rf_probs <- predict(rf_best, newdata = test, type = "prob")[,2]
  
  # Convert labels to binary numeric
  labels <- as.numeric(as.factor(test$Label)) - 1
  
  # Optional: Print label distribution
  cat("Fold", i, "- Label distribution:\n")
  print(table(labels))
  
  # Check that both classes are present before computing ROC
  if (length(unique(labels)) < 2) {
    warning(paste("Fold", i, "has only one class in the test set. Skipping ROC."))
    next
  }
  
  # Compute and store ROC
  roc_obj_rf <- roc(labels, rf_probs)
  roc_list_rf[[i]] <- roc_obj_rf
}

# Plot ROC curves for all folds
png("Balanced accurycy and ROC results/rf_ROC_all_folds.png", width = 800, height = 600, res = 150)
plot(roc_list_rf[[1]], main = "ROC - Random Forest (All 5 Folds)", col = 1, lwd = 2)
for (i in 2:length(roc_list_rf)) {
  plot(roc_list_rf[[i]], add = TRUE, col = i, lwd = 2)
}
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = paste("Fold", 1:length(roc_list_rf)), col = 1:length(roc_list_rf), lwd = 2)
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

# --- 1. BOXPLOT (Top 10 XGBoost Combos by Balanced Accuracy) ---

library(dplyr)
library(ggplot2)

# Add combo label
all_xgb_performance_flat$combo <- paste0(
  "n", all_xgb_performance_flat$nrounds,
  "_d", all_xgb_performance_flat$max_depth,
  "_eta", all_xgb_performance_flat$eta,
  "_a", all_xgb_performance_flat$reg_alpha,
  "_g", all_xgb_performance_flat$reg_gamma,
  "_s", all_xgb_performance_flat$subsample
)

# Get top 10 combos based on mean balanced accuracy
top_combos <- all_xgb_performance_flat %>%
  group_by(combo) %>%
  summarise(mean_bal_acc = mean(balanced_accuracy, na.rm = TRUE)) %>%
  arrange(desc(mean_bal_acc)) %>%
  slice_head(n = 10) %>%
  pull(combo)

# Filter data to only top 10 combos
top_data <- filter(all_xgb_performance_flat, combo %in% top_combos)

# Create output folder to save plot
dir.create("xgb_boxplots", showWarnings = FALSE)

# Create and save plot
png("xgb_boxplots/boxplot_top10_xgboost.png", width = 800, height = 600, res = 150)

ggplot(top_data, aes(x = combo, y = balanced_accuracy)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Top 10 XGBoost Combos by Balanced Accuracy",
       x = "XGBoost Hyperparameter Combo",
       y = "Balanced Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

dev.off()


# --- 2. BOXPLOT ---

# Add combo label
all_xgb_performance_flat$combo <- paste0(
  "n", all_xgb_performance_flat$nrounds,
  "_d", all_xgb_performance_flat$max_depth,
  "_eta", all_xgb_performance_flat$eta,
  "_a", all_xgb_performance_flat$reg_alpha,
  "_g", all_xgb_performance_flat$reg_gamma,
  "_s", all_xgb_performance_flat$subsample
)

# Get unique combos and split into chunks of 20
combos <- unique(all_xgb_performance_flat$combo)
combo_chunks <- split(combos, ceiling(seq_along(combos) / 20))

# Create output folder to save plots
dir.create("xgb_boxplots", showWarnings = FALSE)

# Loop through chunks: plot and save
for (i in seq_along(combo_chunks)) {
  chunk <- combo_chunks[[i]]
  subset_data <- filter(all_xgb_performance_flat, combo %in% chunk)
  
  p <- ggplot(subset_data, aes(x = combo, y = balanced_accuracy)) +
    geom_boxplot(fill = "skyblue", color = "black") +
    labs(title = paste("Balanced Accuracy - XGBoost Chunk", i),
         x = "XGBoost Hyperparameter Combo",
         y = "Balanced Accuracy") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
  
  print(p)  # Show plot in viewer
  
  # Save plot to the xgb_boxplots folder
  ggsave(
    filename = paste0("xgb_boxplots/boxplot_chunk_", i, ".png"),
    plot = p, width = 10, height = 6
  )
}


# --- 3. BEST PARAMS + ROC CURVE ---
# Get best hyperparameters based on average balanced accuracy
xgb_summary <- all_xgb_performance_flat %>%
  group_by(nrounds, max_depth, eta, reg_alpha, reg_gamma, subsample) %>%
  summarise(mean_bal_acc = mean(balanced_accuracy), .groups = "drop") %>%
  arrange(desc(mean_bal_acc))

cat("Best XGBoost hyperparameter combination by balanced accuracy:\n")
print(xgb_summary[1, ])

# Extract best hyperparameters
best_params <- xgb_summary[1, ]

# Initialize list to store ROC objects
roc_list_xgb <- list()

# Loop over 5 folds
for (i in 1:5) {
  # Load training and test data
  train <- read_excel(paste0("Adding_column_output/training_dataset_final_fold_", i, ".xlsx"))
  test <- read_excel(paste0("Adding_column_output/testing_dataset_final_fold_", i, ".xlsx"))
  
  train <- as.data.frame(train); test <- as.data.frame(test)
  train$Sample <- NULL; test$Sample <- NULL
  train$Node_status <- as.factor(train$Node_status)
  test$Node_status <- as.factor(test$Node_status)
  
  colnames(train)[colnames(train) == "Node_status"] <- "Label"
  colnames(test)[colnames(test) == "Node_status"] <- "Label"
  
  # Clean column names
  colnames(train) <- clean_names(colnames(train))
  colnames(test) <- clean_names(colnames(test))
  
  # Ensure binary 0/1 labels
  train_labels <- as.numeric(as.factor(train$Label)) - 1
  test_labels <- as.numeric(as.factor(test$Label)) - 1
  
  # Create data matrices
  train_matrix <- as.matrix(train[, setdiff(names(train), "Label")])
  test_matrix <- as.matrix(test[, setdiff(names(test), "Label")])
  dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
  dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)
  
  # Fit model
  set.seed(123)
  xgb_model <- xgboost(
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
  
  # Predict and store ROC
  xgb_probs <- predict(xgb_model, dtest)
  roc_obj <- roc(test_labels, xgb_probs)
  roc_list_xgb[[i]] <- roc_obj
}

# Plot all ROC curves
png("Balanced accurycy and ROC results/xgb_ROC_all_folds.png", width = 800, height = 600, res = 150)
plot(roc_list_xgb[[1]], col = 1, lwd = 2, main = "ROC - XGBoost (All 5 Folds)")
for (i in 2:5) {
  plot(roc_list_xgb[[i]], add = TRUE, col = i, lwd = 2)
}
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = paste("Fold", 1:5), col = 1:5, lwd = 2)
dev.off()



# Boxplot of average ROC values for the three MLs
# 1. Calculate the average ROC/AUC for each fold and algorithm
mean_auc_lr <- sapply(roc_list_lr, auc)
mean_auc_rf <- sapply(roc_list_rf, auc)
mean_auc_xgb <- sapply(roc_list_xgb, auc)

# 2. Create a data frame with the results
results_df <- data.frame(
  Algorithm = rep(c("Logistic Regression", "Random Forest", "XGBoost"), each = 5),
  Mean_AUC = c(mean_auc_lr, mean_auc_rf, mean_auc_xgb)
)

# 3. Create the boxplot
png("Balanced accurycy and ROC results/ML_average_ROC_comparison.png", width = 800, height = 600, res = 150)

ggplot(results_df, aes(x = Algorithm, y = Mean_AUC)) +
  geom_boxplot(fill = c("lightgreen", "orange", "skyblue"), color = "black") +
  labs(title = "Boxplot of ROC/AUC values for Different Algorithms",
       x = "Machine Learning Algorithm",
       y = "ROC/AUC value") +
  theme_minimal()

dev.off()

mean(mean_auc_lr)
mean(mean_auc_rf)
mean(mean_auc_xgb)

median(mean_auc_lr)
median(mean_auc_rf)
median(mean_auc_xgb)

# Barplot for ROC of individual folds
# 1. Extract AUC per fold
auc_lr <- sapply(roc_list_lr, auc)
auc_rf <- sapply(roc_list_rf, auc)
auc_xgb <- sapply(roc_list_xgb, auc)

# 2. Create a long-form data frame
results_df <- data.frame(
  Fold = rep(1:5, times = 3),
  Algorithm = rep(c("Logistic Regression", "Random Forest", "XGBoost"), each = 5),
  AUC = c(auc_lr, auc_rf, auc_xgb)
)

# 3. Add mean AUC for each algorithm
mean_auc_df <- aggregate(AUC ~ Algorithm, data = results_df, mean)
mean_auc_df$Fold <- "Mean of all folds"  # Label for the facet

# 4. Combine original and mean data
results_df$Fold <- as.character(results_df$Fold)  # Ensure same type for rbind
full_df <- rbind(results_df, mean_auc_df)

# 5. Plot
png("Balanced accurycy and ROC results/ML_ROC_per_fold_faceted.png", width = 1000, height = 600, res = 150)

ggplot(full_df, aes(x = Algorithm, y = AUC, fill = Algorithm)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Fold, ncol = 3) +
  scale_fill_manual(values = c(
    "Logistic Regression" = "lightgreen",
    "Random Forest" = "orange",
    "XGBoost" = "skyblue"
  )) +
  labs(title = "ROC/AUC by Algorithm for Each Fold",
       x = "Algorithm",
       y = "ROC/AUC value") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

dev.off()
