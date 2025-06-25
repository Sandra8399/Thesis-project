library(readxl)
library(writexl)
library(xgboost)
library(iml)
library(ggplot2)
library(multiMiR)
library(org.Hs.eg.db)
library(ReactomePA)
library(clusterProfiler)

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
importance_plot <- xgb.plot.importance(importance_matrix = importance)

ggplot(importance_plot, aes(x = Importance, y = reorder(Feature, Importance))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Importance") +
  ylab("Feature") +
  ggtitle("XGBoost Feature Importance") +
  theme_minimal()

dim(importance) # 47, 5 => 47 have importance

# Extract importance
miRNAs <- importance$Importance
# Add importance names
names(miRNAs) <- importance$Feature

#Order based on importance (decreasing)
miRNAs <- sort(miRNAs, decreasing = TRUE)
miRNAs_df <- as.data.frame(miRNAs)
colnames(miRNAs_df) <- c("importance_value")
miRNAs_df <- data.frame(names(miRNAs),miRNAs_df)
head(miRNAs_df)
tail(miRNAs_df)

write_xlsx(miRNAs_df, file.path(paste0("miRNA_names.xlsx")))

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
dim(mapping_df)

# Aggregate using max absolute value
mapping_rank_df <- aggregate(importance ~ target_RNA, data = mapping_df, FUN = max)
head(mapping_rank_df)
dim(mapping_rank_df)

# Create vector containing RNA targets and their importance values into a vector
rna_targets_vector <- setNames(mapping_rank_df$importance, mapping_rank_df$target_RNA)
head(rna_targets_vector)

# Map RNA SYMBOL to Entrez IDs
names(rna_targets_vector) <- mapIds(org.Hs.eg.db,
                                    keys = names(rna_targets_vector),
                                    column = "ENTREZID",
                                    keytype = "SYMBOL",
                                    multiVals = "first")


head(rna_targets_vector)
length(rna_targets_vector)

# Identify and remove NA values
sum(is.na(names(rna_targets_vector)))
rna_targets_vector<-rna_targets_vector[!is.na(names(rna_targets_vector))]

# Identify and remove NULL values
length(rna_targets_vector[names(rna_targets_vector)=="NULL"])
rna_targets_vector<-rna_targets_vector[!names(rna_targets_vector)=="NULL"]

# Final number of RNA targets to be used as input for GSEA
length(rna_targets_vector)

# Sort by decreasing
rna_targets_vector <- sort(rna_targets_vector, decreasing = TRUE)
# Ensure names are character
names(rna_targets_vector) <- as.character(names(rna_targets_vector))


# Run GSEA with Reactome Database
set.seed(123)
gsea_results <- gsePathway(
  geneList = rna_targets_vector,
  organism = "human",
  pvalueCutoff = 0.1,
  pAdjustMethod = "BH",
  verbose = FALSE,
  eps=0,
  seed=TRUE,
  scoreType="pos"
)

head(gsea_results@result)      # View top pathways
#barplot(gsea_results, showCategory = 10)   # Visualize top 10 pathways

# Run GSEA with Gene Ontology database (Biological Processes)
set.seed(123)
gsea_results_go <- gseGO(geneList = rna_targets_vector,
                         ont = "BP",  
                         OrgDb = org.Hs.eg.db,
                         keyType = "ENTREZID",
                         minGSSize = 1,
                         pvalueCutoff = 0.1,
                         verbose = FALSE,
                         pAdjustMethod = "BH",
                         seed = TRUE,
                         eps=0,
                         by = "fgsea",
                         scoreType="pos")

# Run GSEA with KEGG database 
set.seed(123)
gsea_results_kegg  <- gseKEGG(geneList = rna_targets_vector ,
                              organism = "hsa", 
                              keyType = "kegg",
                              minGSSize = 1,
                              pvalueCutoff = 0.1,
                              verbose = FALSE,
                              pAdjustMethod = "BH",
                              seed = TRUE,
                              eps=0,
                              by = "fgsea",
                              scoreType="pos")


head(gsea_results_kegg)
head(gsea_results_go)
head(gsea_results@result)   







