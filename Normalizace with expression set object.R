library(readxl)
library(writexl)
library(oligo)
library(Biobase)   # To work with ExpressionSet objects

excel_data <- read_excel("cleaned_dataset.xlsx")

expression_matrix <- as.matrix(excel_data[, -1])  # Remove first column (gene identifiers) for the data matrix
rownames(expression_matrix) <- excel_data[[1]]

feature_info <- data.frame(gene_name = rownames(expression_matrix))  # Example, adapt based on your data
featureData <- AnnotatedDataFrame(feature_info)


eset <- ExpressionSet(assayData = expression_matrix,
                      featureData = featureData)