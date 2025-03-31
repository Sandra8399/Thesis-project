library(readxl)
library(writexl)

# Load dataset
training_data <- read_excel("normalized_training_dataset.xlsx")
testing_data <- read_excel("normalized_testing_dataset.xlsx")

gene_symbol_train <- training_data[[1]]
gene_symbol_test <- testing_data[[1]]

# Store original column names (excluding the first column)
training_colnames <- colnames(training_data)[-1]

#need both testing & training on scaling testing, how?
# Apply z-standardization (excluding the first column)

library(matrixStats)
Z_standardization<-function(x){
  means<-rowMeans(x)
  sds<-rowSds(as.matrix(x))
  z<-(x-means) / sds
  return(list(means_train=means,sds_train=sds,train_scaled=z))
}
training_scaled <- Z_standardization(training_data[-1])

#subtract mean/sd of train set

testing_scaled<-as.matrix((testing_data[-1] - training_scaled[["means_train"]]) 
                                     / training_scaled[["sds_train"]])

#convert both datasets back to data frame
training_scaled <- as.data.frame(training_scaled)
testing_scaled <- as.data.frame(testing_scaled)

#remove the mean and sd columns from the training dataset
training_scaled <- training_scaled[, -c(1:2)]

#return the names of the columns to the original
colnames(training_scaled) <- training_colnames

#add back gene symbol
training_scaled$Gene_symbol <- gene_symbol_train
testing_scaled$Gene_symbol <- gene_symbol_test

# Reorder columns to have gene names as the first column
training_scaled <- training_scaled[, c("Gene_symbol", setdiff(colnames(training_scaled), "Gene_symbol"))]
testing_scaled <- testing_scaled[, c("Gene_symbol", setdiff(colnames(testing_scaled), "Gene_symbol"))]

head(training_scaled)

summary(training_scaled)

#histogram
hist(training_data$`UTSW#32-IIIC`)
hist(training_scaled$`UTSW#32-IIIC`)
#density plot
plot(density(training_data$`UTSW#32-IIIC`))
plot(density(training_scaled$`UTSW#32-IIIC`))
#both show mean, distribution is unchanged


write_xlsx(training_scaled, "training_scaled_dataset.xlsx")
write_xlsx(testing_scaled, "testing_scaled_dataset.xlsx")


