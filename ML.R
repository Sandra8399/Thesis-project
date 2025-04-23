# Import necessary libraries
library(readxl)
library(writexl)
library(tidymodels)
library(glmnet)
library(caret)

# Load datasets
train_data_final <- read_excel("training_dataset_final.xlsx")
test_data_final <- read_excel("testing_dataset_final.xlsx")

# Make sure both datasets are dataframe
train_data_final <- as.data.frame(train_data_final)
test_data_final <- as.data.frame(test_data_final)

# Make sure Label is a factor
train_data_final$Label <- as.factor(train_data_final$Label)
test_data_final$Label <- as.factor(test_data_final$Label)

# Test hyper parameters
# Logistic regession

# Define training control with random search
ctrl <- trainControl(method = "cv", number = 5, search = "random")

# Train logistic regression
set.seed(123)
model_glm <- train(Label ~ ., data = train_data_final,
                   method = "glm",
                   family = "binomial",
                   trControl = ctrl,
                   tuneLength = 10)  # number of random combos

summary(model_glm)
model_glm$results
model_glm$bestTune
model_glm$finalModel
#plot(model_glm)

predict(model_glm, newdata = test_data_final)


# Random forest
set.seed(123)
model_rf <- train(Label ~ ., data = train_data_final,
                  method = "rf",
                  trControl = ctrl,
                  tuneLength = 10)  # random 10 sets of mtry

print(model_rf)


# XGBoost
set.seed(123)
model_xgb <- train(Label ~ ., data = train_data_final,
                   method = "xgbTree",
                   trControl = ctrl,
                   tuneLength = 10)  # randomly sample 10 param combos

print(model_xgb)







# Default
# Logistic regression

logistic_regression_train <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(Label ~ ., data = train_data_final)

# Print model summary
summary(logistic_regression_train$fit)
dim(train_data_final)
dim(test_data_final)
#package, library
lr_prediction_test <- predict(logistic_regression_train, new_data = test_data_final)

# Compare predicted results to real data
summary(lr_prediction_test)
table(test_data_final$Label)

#Confusion matrix
confusionMatrix(data=lr_prediction_test$.pred_class, reference = test_data_final$Label)


# Remove all "-" characters and replace all "*" characters, so that RF can work
clean_names <- function(x) {
  x <- gsub("[^A-Za-z0-9*]", "", x)  # Remove all special characters except *
  x <- gsub("\\*", "2", x)           # Replace * with 2
  return(x)
}

# Columns & rows replacement
colnames(train_data_final) <- clean_names(colnames(train_data_final))
rownames(train_data_final) <- clean_names(rownames(train_data_final))

# Repeat for testing
colnames(test_data_final) <- clean_names(colnames(test_data_final))
rownames(test_data_final) <- clean_names(rownames(test_data_final))


#remove all - & * (dupes) from the genes, samples etc (special characters)
#Random forest
library(randomForest)
set.seed(123)
random_forest_train <- randomForest(Label ~ .,
                                    data = train_data_final)
summary(random_forest_train)

rf_prediction_test <- predict(random_forest_train, newdata = test_data_final)

summary(rf_prediction_test)
table(test_data_final$Label)

#Confusion matrix
confusionMatrix(data=rf_prediction_test, reference = test_data_final$Label)

#XGBoost
library(xgboost)

# Prepare for XGBoost
# Extract labels
train_labels <- as.numeric(as.factor(train_data_final$Label)) - 1
test_labels  <- as.numeric(as.factor(test_data_final$Label)) - 1

# Extract features (remove the label column)
train_matrix <- as.matrix(train_data_final[, setdiff(names(train_data_final), "Label")])
test_matrix  <- as.matrix(test_data_final[, setdiff(names(test_data_final), "Label")])

#head(train_matrix)
#class(train_matrix)
#class(train_matrix$kshviRK1210a)
#class(train_matrix$Label)

# Create DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest  <- xgb.DMatrix(data = test_matrix, label = test_labels)

getinfo(dtrain, "label")  # returns the labels
dim(dtrain)               # returns dimensions


# Run XGBoost
xgb_model <- xgboost(
  data = dtrain,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "logloss"
)

# Predict probabilities on the test set
xgb_prediction_test <- predict(xgb_model, dtest)

prediction <- as.numeric(xgb_prediction_test > 0.5)
print(head(prediction))

summary(prediction)
table(test_data_final$Label)

#Confusion matrix
# Labels as 0/1
labels <- as.numeric(as.factor(test_data_final$Label)) - 1

# Confusion matrix
confusionMatrix(
  data = factor(prediction, levels = c(0, 1)),
  reference = factor(labels, levels = c(0, 1))
)
