library(preprocessCore)
library(readxl)
library(writexl)

# Load dataset
excel_data <- read_excel("cleaned_dataset.xlsx")
excel_data <- as.data.frame(excel_data)

# Store gene symbols before removing them
row.names(excel_data) <- excel_data[[1]]
excel_data <- excel_data[, -1]
head(excel_data)

# Function to split dataset into training (70%) and testing (30%)
split_dataset <- function(df, train_ratio = 0.7) {
  set.seed(123)
  total_rows <- nrow(df)
  train_indices <- sample(total_rows, size = round(train_ratio * total_rows))
  
  train_set <- df[train_indices, , drop = FALSE]  
  test_set <- df[-train_indices, , drop = FALSE]  
  
  return(list(train = train_set, test = test_set))
}

# Function for manual quantile normalization
quantile_normalization <- function(train_df, test_df) {
  
  # Step 1: Sort each column and store original row indices
  sorted_ord <- apply(train_df, 2, order)  # Get sorted indices
  sorted_data <- apply(train_df, 2, sort)  # Sort values
  
  # Step 2: Compute row-wise means
  row_means <- rowMeans(sorted_data)
  
  # Step 3: Assign normalized values back to original training ranks
  normalized_train <- matrix(row_means[sorted_ord], ncol=ncol(train_df))
  dimnames(normalized_train) <- dimnames(train_df)
  
  #Check if step 3 worked
  print(sorted_data[1:6,1:6])
  print(sorted_ord[1:6,1:6])
  print(normalized_train[1:6,1:6])

  # Step 4: Normalize the test set using interpolation
  sorted_ord_test <- apply(test_df, 2, order)  # Get sorted indices
  sorted_data_test <- apply(test_df, 2, sort)#row means train data
  
  normalized_test <- matrix(row_means[sorted_ord_test], ncol=ncol(train_df))
  dimnames(normalized_test) <- dimnames(test_df)
  
  #Check if step 4 worked
  print(sorted_data_test[1:6,1:6])
  print(sorted_ord_test[1:6,1:6])
  print(normalized_test[1:6,1:6])
  
  # Convert back to data frames and reattach gene symbols
  train_genes <- row.names(train_df)
  test_genes <- row.names(test_df)
  
  normalized_train_df <- data.frame(Gene = train_genes, normalized_train)
  normalized_test_df <- data.frame(Gene = test_genes, normalized_test)
  
  # Set the correct column names for the normalized data frames
  #"gene needed, otherwise gene symbol names infiltrate the actual dataset"
  colnames(normalized_train_df) <- c("Gene", colnames(train_df))  # Ensure first column is "Gene"
  colnames(normalized_test_df) <- c("Gene", colnames(test_df))    # Ensure first column is "Gene"
  
  return(list(train = normalized_train_df, test = normalized_test_df))
}

# Split dataset
split_data <- split_dataset(excel_data)

# Apply manual quantile normalization
normalized_data <- quantile_normalization(split_data$train, split_data$test)

# Check output
head(normalized_data$train)

# Boxplot before and after normalization
boxplot(as.matrix(excel_data), main = "Before Normalization")
boxplot(as.matrix(normalized_data$train[,-1]), main = "After Normalization")

# Export data
write_xlsx(normalized_data$train, "normalized_training_dataset.xlsx")
write_xlsx(normalized_data$test, "normalized_testing_dataset.xlsx")
