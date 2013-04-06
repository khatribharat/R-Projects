# SVM classification 
{
library(e1071);
library(foreign);
try(dataFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(output <- scan(what = character(), nmax = 1), silent = FALSE);
dataSet <- read.arff(dataFile);
split <- round(.34*dim(dataSet)[1]);
idx <- sample(1:dim(dataSet)[1], split);
training <- dataSet[-idx,];
test <- dataSet[idx,];
model <- svm(class ~ ., data = training);
# predict the results on test dataset
prediction <- predict(model, test[,-dim(test)[2]]);
# preparing the confusion matrix
tab <- table(pred = prediction, true = test[,dim(test)[2]]);

# write the results in an output file
cat(prediction, tab, file = output, append = TRUE, sep = "\n");

# plot the SVM decision boundary
plot(model, training);
}
