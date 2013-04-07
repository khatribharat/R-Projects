# naiveBayes classification 
{
library(e1071);
library(foreign);
print("Usage: <source.R> <data file> <output file> <number of trials>");
try(dataFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(outputFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(numTrials <- scan(what = integer(), nmax = 1), silent = FALSE);
dataSet <- read.arff(dataFile);
split <- round(.34*dim(dataSet)[1]);
for (i in 1:numTrials) {
	idx <- sample(1:dim(dataSet)[1], split);
	training <- dataSet[-idx,];
	test <- dataSet[idx,];
	executionTime <- system.time(model <- naiveBayes(class ~ ., data = training, laplace = 1));
	# predict the results on test dataset
	prediction <- predict(model, test[,-dim(test)[2]]);
	# preparing the confusion matrix
	tab <- table(pred = prediction, true = test[,dim(test)[2]]);
	# print the results in the output file
	write.table(tab, file = outputFile, append = TRUE);
	cat(c("\nExecution Time:",executionTime[3], "\n\n"), file = outputFile, append = TRUE);
}
}
