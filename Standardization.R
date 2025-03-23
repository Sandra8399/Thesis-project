library(readxl)
library(writexl)

# Load dataset
training_data <- read_excel("normalized_training_dataset.xlsx")
testing_data <- read_excel("normalized_testing_dataset.xlsx")

gene_symbol_train <- training_data[[1]]
gene_symbol_test <- testing_data[[1]]

#need both testing & training on scaling testing, how?
# Apply z-standardization (excluding the first column)
training_scaled <- as.data.frame(scale(training_data[-1]))  # Scaling the training data
testing_scaled <- as.data.frame(scale(testing_data[-1]))    # Scaling the testing data

#add back gene symbol
training_scaled$Gene_symbol <- gene_symbol_train
testing_scaled$Gene_symbol <- gene_symbol_test

# Reorder columns to have gene names as the first column
training_scaled <- training_scaled[, c("Gene_symbol", setdiff(colnames(training_scaled), "Gene_symbol"))]
testing_scaled <- testing_scaled[, c("Gene_symbol", setdiff(colnames(testing_scaled), "Gene_symbol"))]


head(training_scaled)

summary(training_scaled)

write_xlsx(training_scaled, "training_scaled_dataset.xlsx")
write_xlsx(testing_scaled, "testing_scaled_dataset.xlsx")

