library(readxl)
library(data.table)
library(writexl)
library(oligo)

# Read the Excel file
excel_data <- read_excel("cleaned_dataset.xlsx")

# Convert it to a feature object (data.table)
feature_data <- as.data.table(excel_data)

# View the feature object
head(feature_data)

# Apply RMA normalization
normalized_data <- rma(feature_data)

#visualize the difference before and after normalization
hist(clean_dataset)
hist(normalized_dataset)

plot_colors_treatment <- rep(c("red", "blue"), 7)
plot_colors_treatment

par(mfrow=c(1,2))
par(cex.axis=0.5)
boxplot(log2(clean_dataset+1),col = plot_colors_treatment,las = 2,main = "Non-normalized")
par(cex.axis=0.5)
boxplot(assay(normalized_dataset),col = plot_colors_treatment,las = 2,main = "Normalized")





















