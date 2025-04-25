library(readxl)
library(writexl)
library(matrixStats)

# Specify what folder to draw from
input_directory <- "Quantile_normalization_output"

for (i in 1:5) {
  cat("Processing Z-score standardization for Fold", i, "\n")
  
  # Load training and testing data from Excel files
  training_data <- read_excel(file.path(input_directory, paste0("normalized_train_fold_", i, ".xlsx")))
  testing_data <- read_excel(file.path(input_directory, paste0("normalized_test_fold_", i, ".xlsx")))
  
  # Extract gene symbols
  gene_symbol_train <- training_data[[1]]
  gene_symbol_test <- testing_data[[1]]
  
  # Store original column names (excluding gene symbol column)
  training_colnames <- colnames(training_data)[-1]
  
  # Z-score standardization function
  Z_standardization <- function(x) {
    means <- rowMeans(x)
    sds <- rowSds(as.matrix(x))
    z <- (x - means) / sds
    return(list(means_train = means, sds_train = sds, train_scaled = z))
  }
  
  # Apply to training data
  training_scaled <- Z_standardization(training_data[-1])
  
  # Apply to test using training means/sds
  testing_scaled <- as.matrix((testing_data[-1] - training_scaled[["means_train"]]) / training_scaled[["sds_train"]])
  
  # Convert to data frames
  training_scaled <- as.data.frame(training_scaled)
  testing_scaled <- as.data.frame(testing_scaled)
  
  # Remove means/sds
  training_scaled <- training_scaled[, -c(1:2)]
  
  # Restore column names
  colnames(training_scaled) <- training_colnames
#  colnames(testing_scaled) <- colnames(testing_data)[-1]
  
  # Add gene symbols
  training_scaled$Gene_symbol <- gene_symbol_train
  testing_scaled$Gene_symbol <- gene_symbol_test
  
  # Reorder columns
  training_scaled <- training_scaled[, c("Gene_symbol", setdiff(names(training_scaled), "Gene_symbol"))]
  testing_scaled <- testing_scaled[, c("Gene_symbol", setdiff(names(testing_scaled), "Gene_symbol"))]
  
  # Plotting histograms and density plots with axis labels
  gene_to_plot <- "UTSW#01-IC"  # Replace with any gene column you'd like to visualize
  
  # Histogram before standardization
  hist(training_data[[gene_to_plot]],
       main = paste("Histogram of", gene_to_plot, "- Fold", i, "(Before Z-Score)"),
       xlab = "Expression Value",
       ylab = "Frequency",
       col = "lightblue")
  
  # Histogram after standardization
  hist(training_scaled[[gene_to_plot]],
       main = paste("Histogram of", gene_to_plot, "- Fold", i, "(After Z-Score)"),
       xlab = "Z-Score",
       ylab = "Frequency",
       col = "lightgreen")
  
  # Density before standardization
  plot(density(training_data[[gene_to_plot]]),
       main = paste("Density of", gene_to_plot, "- Fold", i, "(Before Z-Score)"),
       xlab = "Expression Value",
       ylab = "Density",
       col = "blue",
       lwd = 2)
  
  # Density after standardization
  plot(density(training_scaled[[gene_to_plot]]),
       main = paste("Density of", gene_to_plot, "- Fold", i, "(After Z-Score)"),
       xlab = "Z-Score",
       ylab = "Density",
       col = "darkgreen",
       lwd = 2)
  
  
  # Save to Excel
  write_xlsx(training_scaled, file.path("Scaling_output", paste0("training_scaled_fold_", i, ".xlsx")))
  write_xlsx(testing_scaled, file.path("Scaling_output", paste0("testing_scaled_fold_", i, ".xlsx")))


               
}


