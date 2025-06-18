library(readxl)
library(writexl)
library(preprocessCore)

# Load dataset
excel_data <- read_excel("cleaned_dataset.xlsx")
excel_data <- as.data.frame(excel_data)

# Store gene symbols before removing them
row.names(excel_data) <- excel_data[[1]]
excel_data <- excel_data[, -1]


class(excel_data)
normalized_data <- normalize.quantiles(as.matrix(excel_data))


dimnames(normalized_data) <- dimnames(excel_data)

# Convert back to data frames and reattach gene symbols
train_genes <- row.names(excel_data)

normalized_data <- data.frame(Gene_symbol = train_genes, normalized_data)

# Set the correct column names for the normalized data frames
colnames(normalized_data) <- c("Gene_symbol", colnames(excel_data))  # Ensure first column is "Gene"


excel_data[1:6,1:6]
normalized_data[1:6,1:6]

# Verify normalization with boxplots
# Boxplot before and after normalization
par(mar = c(10, 6, 4, 2))

boxplot(as.matrix(excel_data),
        main = paste("Before Normalization"),
        ylab = "Expression",
        las = 2)
mtext("Samples", side = 1, line = 8)  # Increase 'line' to push further down


boxplot(as.matrix(normalized_data[,-1]),
        main = paste("After Normalization"),
        ylab = "Expression",
        las = 2)
mtext("Samples", side = 1, line = 8)  # Increase 'line' to push further down


# Z-score standardization
standardized_data <- as.data.frame(scale(normalized_data[-1]))
# Add gene symbols
standardized_data$Gene_symbol <- gene_symbol_train

# Reorder columns
standardized_data <- standardized_data[, c("Gene_symbol", setdiff(names(standardized_data), "Gene_symbol"))]

standardized_data[1:6,1:6]

#Verify normalization
# Plotting histograms and density plots with axis labels
gene_to_plot <- "UTSW#01-IC"  # Replace with any gene column you'd like to visualize

# Histogram before standardization
hist(normalized_data[[gene_to_plot]],
     main = paste("Histogram of", gene_to_plot, "- Fold", i, "(Before Z-Score)"),
     xlab = "Expression Value",
     ylab = "Frequency",
     col = "lightblue")

# Histogram after standardization
hist(standardized_data[[gene_to_plot]],
     main = paste("Histogram of", gene_to_plot, "- Fold", i, "(After Z-Score)"),
     xlab = "Z-Score",
     ylab = "Frequency",
     col = "lightgreen")



# Lables
## === TESTING SET PROCESSING ===

sample_names <- colnames(standardized_data)[-1]
status <- ifelse(grepl("-IIIC$", sample_names), 0, 1)

df <- as.data.frame(t(standardized_data[-1]))
colnames(df) <- standardized_data[[1]]

data_final <- cbind(Sample = sample_names, df, Node_status = status)
data_final$Node_status <- as.factor(data_final$Node_status)

data_final[1:6,1:6]

# Save outputs
write_xlsx(data_final, file.path(paste0("Pre-processed_dataset.xlsx")))




