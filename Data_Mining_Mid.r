mydata <- read.csv("D:/wind_dataset.csv",header = TRUE,sep = ",")
options(max.print = 100000000) 
mydata
View(mydata)

str(mydata)

head(mydata)
tail(mydata)

summary(mydata)

mydata <- mydata[, -which(names(mydata) == "DATE")]

missing_values <- colSums(is.na(mydata))
print(missing_values)

# Replace NA values with column means
mydata <- apply(mydata, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
mydata


View(mydata)

missing_values <- colSums(is.na(mydata))
print(missing_values)




# Step 1: Normalize the dataset
numeric_columns <- sapply(mydata, is.numeric)
mydata[numeric_columns] <- scale(mydata[numeric_columns])

# Step 2: Set up predictor variables and target variable
# Identify predictor variables (all columns except the target variable)
predictor_column_names <- setdiff(names(mydata), "WIND")

# Extract predictor variables
predictors <- mydata[, predictor_column_names]

# Identify the target variable
targetumn_name <- "WIND"

# Extract the target variable
target <- mydata[, targetumn_name]

str(mydata)
View(mydata)
head(mydata)
tail(mydata)



# Setting predictor variables and the target variable
predictors <- names(mydata)[-ncol(mydata)]
target <- names(mydata)[ncol(mydata)]

set.seed(123)
train_indices <- sample(1:nrow(mydata), round(0.7 * nrow(mydata)))
train_data <- mydata[train_indices, predictors]
train_labels <- mydata[train_indices, target]
test_data <- mydata[-train_indices, predictors]
test_labels <- mydata[-train_indices, target]
str(train_data)
str(test_data)

library(class)
library(ggplot2)
# Set the value of k (number of neighbors) and distance measures

knn_with_distance_measure <- function(train_data, test_data, train_labels, k,
                                      distance_measure) {
  predicted_labels <- knn(train = train_data, test = test_data, cl = train_labels, k = k, prob =
                            TRUE, use.all = TRUE)
  return(predicted_labels)
}



# Set the values of k
k_values <- c(3, 5, 7)
# Initialize vectors to store accuracies
accuracies <- vector()
# Apply k-NN for each k value and distance measure
for (k in k_values) {
  # Apply k-NN with Euclidean distance
  euclidean_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                     "euclidean")
  # Apply k-NN with Manhattan distance
  manhattan_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                     "manhattan")
  # Apply k-NN with Maximum distance
  maximum_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                   "maximum")
  # Evaluate the accuracy of the predictions
  accuracy_euclidean <- sum(euclidean_predictions == test_labels) / length(test_labels)
  accuracy_manhattan <- sum(manhattan_predictions == test_labels) / length(test_labels)
  accuracy_maximum <- sum(maximum_predictions == test_labels) / length(test_labels)
  # Store the accuracy
  accuracies <- c(accuracies, accuracy_euclidean, accuracy_manhattan, accuracy_maximum)
  # Print the accuracy for the current k value
  cat("Accuracy for k =", k, "\n")
  cat("Euclidean Distance:", accuracy_euclidean, "\n")
  cat("Manhattan Distance:", accuracy_manhattan, "\n")
  cat("Maximum Distance:", accuracy_maximum, "\n")
  cat("\n")
}


# Create a data frame for accuracies
accuracy_df <- data.frame(Distance = rep(c("Euclidean", "Manhattan", "Maximum"),
                                         length(k_values)),
                          K = rep(k_values, each = 3),
                          Accuracy = accuracies)
accuracy_df
