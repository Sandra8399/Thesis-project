library(preprocessCore)
library(readxl)
library(writexl)

excel_data <- read_excel("cleaned_dataset.xlsx")

# Function to split dataset into training (70%) and testing (30%)
split_dataset <- function(df, train_ratio = 0.7) {
  set.seed(123)  # Set seed for reproducibility
  total_rows <- nrow(df)
  train_indices <- sample(total_rows, size = round(train_ratio * total_rows))  # Randomly sample 70%
  
  train_set <- df[train_indices, ]  # Training set
  test_set <- df[-train_indices, ]  # Testing set
  
  return(list(train = train_set, test = test_set))
}

# Function for manual quantile normalization
quantile_normalization <- function(train_df, test_df) {
  # Extract gene symbols (first column)
  train_genes <- train_df[, 1, drop = FALSE]
  test_genes <- test_df[, 1, drop = FALSE]
  
  # Convert numeric data to matrix (excluding gene symbols)
  train_data <- as.matrix(train_df[, -1])
  test_data <- as.matrix(test_df[, -1])
  
  # Step 1: Sort each column and store original row indices
  sorted_idx <- apply(train_data, 2, order)
  sorted_data <- apply(train_data, 2, sort)
  
  # Step 2: Compute row-wise means for quantile normalization
  row_means <- rowMeans(sorted_data)
  
  # Step 3: Assign normalized values back to original training ranks
  normalized_train <- matrix(0, nrow = nrow(train_data), ncol = ncol(train_data))
  for (col in 1:ncol(train_data)) {
    normalized_train[sorted_idx[, col], col] <- row_means
  }
  
  # Step 4: Apply the same transformation to the test set
  normalized_test <- matrix(0, nrow = nrow(test_data), ncol = ncol(test_data))
  for (col in 1:ncol(test_data)) {
    ranks <- rank(test_data[, col], ties.method = "average")  # Get ranks in test data
    normalized_test[, col] <- approx(1:length(row_means), row_means, xout = ranks, rule = 2)$y
  }
  
  # Convert back to data frames and reattach gene symbols
  normalized_train_df <- data.frame(train_genes, normalized_train)
  normalized_test_df <- data.frame(test_genes, normalized_test)
  
  # Preserve column names
  colnames(normalized_train_df) <- colnames(train_df)
  colnames(normalized_test_df) <- colnames(test_df)
  
  return(list(train = normalized_train_df, test = normalized_test_df))
}


# Split the dataset into training (70%) and testing (30%)
split_data <- split_dataset(excel_data)

# Apply manual quantile normalization
normalized_data <- quantile_normalization(split_data$train, split_data$test)

head(normalized_df)
#boxplot before and after to verify if normalization worked
boxplot(excel_data[,-1])
boxplot(normalized_df[,-1])
