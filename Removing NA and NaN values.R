#THIS WORKS, BUT WE NEED 2 REMOVE SOME ROWS MANUALLY

library(readxl)
library(dplyr)
library(writexl)

#import dataset
dataset <- read_excel('Book2.xlsx')
class(dataset$Probes)
class(dataset$`Gene Symbol`)
class(dataset$hsa_mirbase)
class(dataset$`UTSW#01-IC`)

dataset[] <- lapply(dataset, function(x) {
  if (is.character(x)) {
    # Only apply the transformation to character columns
    return(as.numeric(gsub(",", ".", x)))
  } else {
    # Leave non-character columns as is
    return(x)
  }
})

#dataset
#dataset[] <- lapply(dataset, function(x) as.numeric(as.character(x)))

dataset[] <- lapply(dataset, function(x) {
  if (is.numeric(x)) x[is.nan(x)] <- NA  # Convert NaN to NA
  return(x)
})
sum(is.nan(as.matrix(dataset)))

#dataset <- na.omit(dataset)
#remove columns containing NAs
#define new data frame
new_daset <- dataset[, colSums(is.na(dataset)) == 0]

#view new data frame
new_daset

dim(dataset)
dim(new_daset)

#verify that all NA values have been removed
sum(is.na(new_daset))
colSums(is.na(new_daset))




# Export the cleaned dataset to an Excel file
write_xlsx(new_daset, "cleaned_dataset.xlsx")
           
           