library(readxl)
library(writexl)

# Load dataset
excel_data <- read_excel("cleaned_dataset.xlsx")
excel_data <- as.data.frame(excel_data)

# Store gene symbols before removing them
row.names(excel_data) <- excel_data[[1]]
excel_data <- excel_data[, -1]

set.seed(123)

test_set <- list()
train_set <- list()

n_total_cols <- ncol(excel_data)
col_indices <- 1:n_total_cols

# Shuffle column indices for reproducibility
shuffled_indices <- sample(col_indices)

# Split into 5 roughly equal folds
folds <- split(shuffled_indices, cut(seq_along(shuffled_indices), 5, labels = FALSE))

for (i in 1:5) {
  test_indices <- folds[[i]]
  train_indices <- setdiff(col_indices, test_indices)
  
  test_set[[i]] <- excel_data[, test_indices, drop = FALSE]
  train_set[[i]] <- excel_data[, train_indices, drop = FALSE]
}


# Function for manual quantile normalization
quantile_normalization <- function(train_df, test_df) {
  
  # Step 1: Sort each column and store original row indices
  sorted_ord <- apply(train_df, 2, order)  # Get sorted indices
  sorted_data <- apply(train_df, 2, sort)  # Sort values
  
  # Step 2: Compute row-wise means
  row_means <- rowMeans(sorted_data)
  
  # Step 3: Assign normalized values back to original training ranks
  normalized_train <- matrix(row_means[sorted_ord], ncol = ncol(train_df))
  dimnames(normalized_train) <- dimnames(train_df)
  
  # Step 4: Normalize the test set using the same row means
  sorted_ord_test <- apply(test_df, 2, order)  # Get sorted indices for the test set
  sorted_data_test <- apply(test_df, 2, sort)  # Sort test set data
  
  normalized_test <- matrix(row_means[sorted_ord_test], ncol = ncol(test_df))
  dimnames(normalized_test) <- dimnames(test_df)
  
  # Convert back to data frames and reattach gene symbols
  train_genes <- row.names(train_df)
  test_genes <- row.names(test_df)
  
  normalized_train_df <- data.frame(Gene_symbol = train_genes, normalized_train)
  normalized_test_df <- data.frame(Gene_symbol = test_genes, normalized_test)
  
  # Set the correct column names for the normalized data frames
  colnames(normalized_train_df) <- c("Gene_symbol", colnames(train_df))  # Ensure first column is "Gene"
  colnames(normalized_test_df) <- c("Gene_symbol", colnames(test_df))    # Ensure first column is "Gene"
  
  return(list(train = normalized_train_df, test = normalized_test_df))
}

# Loop over the 5 folds and apply quantile normalization to each fold
for (i in 1:5) {
  cat("Normalizing fold", i, "\n")
  
  # Get the current train and test set for the fold
  train_df <- as.matrix(train_set[[i]])
  test_df <- as.matrix(test_set[[i]])
  
  # Apply manual quantile normalization
  normalized_data <- quantile_normalization(train_df, test_df)
  
  # Check output (you can remove these print statements later for a cleaner output)
  head(normalized_data$train)
  
  # Boxplot before and after normalization
  par(mar = c(10, 6, 4, 2))
  
  boxplot(as.matrix(train_df),
          main = paste("Before Normalization - Fold", i),
          ylab = "Expression",
          las = 2)
  mtext("Samples", side = 1, line = 8)  # Increase 'line' to push further down
  
  
  boxplot(as.matrix(normalized_data$train[,-1]),
          main = paste("After Normalization - Fold", i),
          ylab = "Expression",
          las = 2)
  mtext("Samples", side = 1, line = 8)  # Increase 'line' to push further down
  
  

  # Export data for the current fold
  

  write_xlsx(normalized_data$train, file.path("Quantile_normalization_output", paste0("normalized_train_fold_", i, ".xlsx")))
  write_xlsx(normalized_data$test, file.path("Quantile_normalization_output", paste0("normalized_test_fold_", i, ".xlsx")))
}