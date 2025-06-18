library(readxl)
library(writexl)
library(xgboost)
library(iml)
library(multiMiR)
library(org.Hs.eg.db)
library(ReactomePA)

# Load dataset
final_data <- read_excel("Pre-processed_dataset.xlsx")
final_data <- as.data.frame(final_data)
colnames <- colnames(final_data)


final_data$Sample  <- NULL
final_data$Node_status  <- as.factor(final_data$Node_status)
colnames(final_data)[colnames(final_data) == "Node_status"] <- "Label"

# Extract label and features
labels  <- as.numeric(as.factor(final_data$Label)) - 1
matrix  <- as.matrix(final_data[, setdiff(names(final_data), "Label")])
dfinal <- xgb.DMatrix(data = matrix, label = labels)

# Fit model
xgb_model <- xgboost(
  data = dfinal,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 50,
  max_depth = 3,
  eta = 0.6,
  reg_alpha = 1,
  reg_gamma = 0,
  subsample = 1,
  verbose = 0
)

# Get importance values
importance <- xgb.importance(feature_names = colnames(matrix), model = xgb_model)
xgb.plot.importance(importance_matrix = importance)

# Extract importance
miRNAs <- importance$Importance
# Add importance names
names(miRNAs) <- importance$Feature

#Order based on importance (decreasing)
miRNAs <- sort(miRNAs, decreasing = TRUE)
# Identify RNA targets of the miRNAs
results <- get_multimir(mirna = names(miRNAs), table = "validated")

# Print results
targets_df <- results@data
head(targets_df)



# RNA targets and the corresponding miRNAs mapped
mapping_df <- data.frame(
  miRNA = targets_df$mature_mirna_id,
  target_RNA = targets_df$target_symbol,
  stringsAsFactors = FALSE
)

# Add importance by matching miRNA names
mapping_df$importance <- miRNAs[mapping_df$miRNA]

# View result
head(mapping_df)


# Create vector containing RNA targets and their importance values into a vector
rna_targets_vector <- setNames(mapping_df$importance, mapping_df$target_RNA)

# Map RNA SYMBOL to Entrez IDs
entrez_ids <- mapIds(org.Hs.eg.db,
                     keys = names(rna_targets_vector),
                     column = "ENTREZID",
                     keytype = "SYMBOL",
                     multiVals = "first")


# Replace RNA SYMBOL with Entrez ID to dataset
names(rna_targets_vector) <- entrez_ids[names(rna_targets_vector)]
# Sort by decreasing
rna_targets_vector <- sort(rna_targets_vector, decreasing = TRUE)
# Ensure names are character
names(rna_targets_vector) <- as.character(names(rna_targets_vector))


# Run GSEA
gsea_results <- gsePathway(
  geneList = rna_targets_vector,
  organism = "human",
  pvalueCutoff = 0.05,
  verbose = TRUE
)

head(gsea_results@result)      # View top pathways
barplot(gsea_results, showCategory = 10)   # Visualize top 10 pathways










