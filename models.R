
#install packages
install.packages("ggplot2")
install.packages("caret")
install.packages("kernlab")
install.packages("rattle")
install.packages("corrplot")
install.packages("randomForest")

# libraries
library(lattice)
library(ggplot2)
library(caret)
library(kernlab)
library(rattle)
library(corrplot)

set.seed(1234)

# load the data
traincsv <- read.csv("/Users/adityakatkuri/Desktop/Practical-Machine-Learning-Project-master/data/pml-training.csv")
testcsv <- read.csv("/Users/adityakatkuri/Desktop/Practical-Machine-Learning-Project-master/data/pml-testing.csv")

# cleaning the data
traincsv <- traincsv[,colMeans(is.na(traincsv)) < .9] #removing mostly na columns
traincsv <- traincsv[,-c(1:7)] #removing metadata which is irrelevant to the outcome
# removing zero variance variables
nvz <- nearZeroVar(traincsv)
traincsv <- traincsv[,-nvz]
dim(traincsv)

# create validation and training set from training dataset
inTrain <- createDataPartition(y=traincsv$classe, p=0.7, list=F)
train <- traincsv[inTrain,]
valid <- traincsv[-inTrain,]

# Run Model
print("Training models, please wait...")
control <- trainControl(method="cv", number=3, verboseIter=F) #set up fixed training parameters

## random forest
mod_rf <- train(classe~., data=train, method="rf", trControl = control, tuneLength = 5)
pred_rf <- predict(mod_rf, valid)
cmrf <- confusionMatrix(pred_rf, factor(valid$classe))

# informing user of each model's evaluation
print("Accuracy and out of sample error rate for random forest model")
models <- c("RF")
accuracy <- c(round(cmrf$overall[1],3)) #accuracy
oos_error <- 1 - accuracy #out of sample error
data.frame(accuracy = accuracy, oos_error = oos_error, row.names = models)

# using best model
print("Using RF model to predict class outcome for each test set observation...")
pred <- predict(mod_rf, testcsv)
print(pred)

print("Done.")

