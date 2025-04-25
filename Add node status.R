library(readxl)
library(writexl)

input_directory <- "Scaling_output"

for (i in 1:5) {
  cat("Processing Fold", i, "\n")
  
  # Load datasets
  train_data <- read_excel(file.path(input_directory, paste0("training_scaled_fold_", i, ".xlsx")))
  test_data  <- read_excel(file.path(input_directory, paste0("testing_scaled_fold_", i, ".xlsx")))
  
  ## === TRAINING SET PROCESSING ===
  
  # Get sample names from column headers
  train_sample_names <- colnames(train_data)[-1]  # exclude 'Gene_symbol'
  
  # Get binary status
  train_status_binary <- ifelse(grepl("-IIIC$", train_sample_names), 0, 1)
  
  # Transpose training data (excluding Gene_symbol column)
  train_data_t <- as.data.frame(t(train_data[-1]))  # removes first column (Gene_symbol)
  colnames(train_data_t) <- train_data[[1]]  # set gene symbols as column names
  
  # Add sample name and status columns
  train_data_finals <- cbind(Sample = train_sample_names, train_data_t, Node_status = train_status_binary)
  train_data_finals$Node_status <- as.factor(train_data_finals$Node_status)
  
  ## === TESTING SET PROCESSING ===
  
  test_sample_names <- colnames(test_data)[-1]
  test_status_binary <- ifelse(grepl("-IIIC$", test_sample_names), 0, 1)
  
  test_data_t <- as.data.frame(t(test_data[-1]))
  colnames(test_data_t) <- test_data[[1]]
  
  test_data_finals <- cbind(Sample = test_sample_names, test_data_t, Node_status = test_status_binary)
  test_data_finals$Node_status <- as.factor(test_data_finals$Node_status)
  
  # Save outputs
  write_xlsx(train_data_finals, file.path("Adding_column_output", paste0("training_dataset_final_fold_", i, ".xlsx")))
  write_xlsx(test_data_finals,  file.path("Adding_column_output", paste0("testing_dataset_final_fold_", i, ".xlsx")))
}

