# Importing libraries
library(e1071)
library(caTools)
library(caret)

# Load the dataset
# Source: https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database
data <- read.csv("diabetes.csv")

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(data$Outcome, p=0.8, list = FALSE)
TrainingSet <- data[TrainingIndex,] # Training Set
TestingSet <- data[-TrainingIndex,] # Test Set
TestingSet$Outcome <- factor(TestingSet$Outcome)

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv")
TrainSet <- TrainSet[,-1]
TrainSet$Outcome <- factor(TrainSet$Outcome) # for classification

# Build naive bayes model
model2 <- naiveBayes(Outcome ~ ., data = TrainSet)

# Predicting on test data'
y_pred <- predict(model2, newdata = TestingSet)

# Confusion Matrix
cm <- table(TestingSet$Outcome, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)

# Save model to RDS file
saveRDS(model2, "model.rds")

