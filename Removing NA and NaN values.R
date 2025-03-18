#THIS WORKS, BUT WE NEED 2 REMOVE SOME ROWS MANUALLY

library(readxl)
library(dplyr)
library(writexl)

#import dataset
dataset <- read_excel('Book.xlsx')
#remove 1st and 3rd column
dataset <- dataset[, -c(1,3)]

dataset[, -1] <- lapply(dataset[, -1], function(x) as.numeric(gsub(",", ".", x)))

#dataset
#dataset[] <- lapply(dataset, function(x) as.numeric(as.character(x)))

dataset[] <- lapply(dataset, function(x) {
  if (is.numeric(x)) x[is.nan(x)] <- NA  # Convert NaN to NA
  return(x)
})
sum(is.nan(as.matrix(dataset)))

#remove rows containing NAs
#define new data frame
new_daset <- na.omit(dataset)


#view new data frame
new_daset

dim(dataset)
dim(new_daset)

#verify that all NA values have been removed
sum(is.na(new_daset))
colSums(is.na(new_daset))

# Export the cleaned dataset to an Excel file
write_xlsx(new_daset, "cleaned_dataset.xlsx")
           
           