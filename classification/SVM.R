# SVM classification 
{
library(e1071);
library(foreign);
print("Usage: <source.R> <data file> <output file> <number of trials>")
try(dataFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(outputFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(numTrials <- scan(what = integer(), nmax = 1), silent = FALSE);
dataSet <- read.arff(dataFile);
split <- round(.34*dim(dataSet)[1]);

for (i in 1:numTrials)
{
	idx <- sample(1:dim(dataSet)[1], split);
	training <- dataSet[-idx,];
	test <- dataSet[idx,];
	executionTime <- system.time(model <- svm(class ~ ., data = training));
	# predict the results on test dataset
	prediction <- predict(model, test[,-dim(test)[2]]);
	# preparing the confusion matrix
	tab <- table(pred = prediction, true = test[,dim(test)[2]]);
	
	# write the results in an output file
	write.table(tab, file = outputFile, append = TRUE);
	cat(c("\nExecution time:", executionTime[3], "\n\n"), file = outputFile, append = TRUE);
	
	#TODO An error is encountered on using the plot function.
	#plot(model, training);
}
}
