library(caret)
library(rattle)

## Loading data
## Load the training and testing data
training <- read.csv(file="pml-training.csv", header=TRUE, na.strings = c("", "NA", "#DIV/0!"))
testing <- read.csv(file="pml-testing.csv", header=TRUE, na.strings = c("", "NA", "#DIV/0!"))

## Number of rows and columns
nrow(training)
ncol(training)

## Exploring data
## A bar chart for the response variable, Classe
barplot(table(training$classe), main = "Classe", ylim = c(0, 6000))

## Managing data
## Remove descriptive variables
dv <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
training <- training[, !(names(training) %in% dv)]

## Remove near zero variables
nzv <- nearZeroVar(training)
training <- training[, -nzv]

## Remove variables with more than 30% of NA
nas <- sapply(training, function(x) { sum(is.na(x)) <= 0.3 * length(training$classe) })
training <- training[, nas]

## Splitting data
## As the number of data is sufficiently large, the validation set approach is used
train <- createDataPartition(training$classe, p = 0.7, list = FALSE)
trainingSet <- training[train, ]
ValidationSet <- training[-train, ]

## Training
## This is a classification problem
## Random forest is chosen to be the modelling method. Random forests are usually one
## of the top performing algorithms in prediction contests.
modFit <- train(classe ~ ., data = trainingSet, method = "rf", prox = TRUE, ntree = 16, allowParallel = TRUE)

## Training Set accuracy
p <- predict(modFit, trainingSet)
confusionMatrix(p, trainingSet$classe)

## Validation Set accuracy
p <- predict(modFit, ValidationSet)
confusionMatrix(p, ValidationSet$classe)

## Testing
p <- predict(modFit, testing)

