##Step 1: LOAD R PACKAGES

library(caret); library(ggplot2); library(rattle); library(dplyr); library(rpart)


##Step 2: IMPORT THE DATA

trainFilename <- "pml-training.csv"; testFilename <- "pml-testing.csv"
trainingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(trainingURL, trainFilename, method="curl")
download.file(testingURL, testFilename, method="curl")

## TRAINING & TEST DATA SETS
trainHAR <- read.csv("pml-training.csv", na.strings = c("NA", ""))
testHAR <- read.csv("pml-testing.csv", na.strings = c("NA", ""))


## Step 3: DATA PARTITIONING
inTrain <- createDataPartition(y=trainHAR$classe, p=0.7, list = FALSE)
training <- trainHAR[inTrain, ]; validation <- trainHAR[-inTrain, ]
dim(training); dim(validation)


## Step 4: CLEANING DATA
training <- training[ , colSums(is.na(training)) == 0] ##empty columns removed
training <- training[ , -c(1:7)] ##remove non-essential columns

validation <- validation[ , colSums(is.na(validation)) == 0] ##empty columns removed
validation <- validation[ , -c(1:7)] ##remove non-essential columns
dim(training); dim(validation)


##Step 5: PREDICTION MODEL 1 -- CLASSIFICATION TREE
modCT <- train(classe ~., method = "rpart", data = training) ##classification tree
fancyRpartPlot(modCT$finalModel) ##plot tree

predCT <- predict(modCT, newdata = validation) ##predict new values
confusionMatrix(predCT, validation$classe) ##check prediction accuracy

##Step 6: PREDICTION MODEL 2 -- RANDOM FOREST
modRF <- train(classe ~., data = training, method = "rf", prox = TRUE)
print(modRF)

predRF <- predict(modRF, newdata = validation)
confusionMatrix(predRF, validation$classe)

##Step 7: TESTING SET
predict(modRF, newdata = testHAR)
