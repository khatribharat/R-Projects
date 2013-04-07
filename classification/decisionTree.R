# Decision Tree
{
findClass <- function(prediction) {
	dimen <- dim(prediction);
	numClasses <- dimen[2];
	numInstances <- dimen[1];
	# modPrediction would later be promoted from the 'integer' class to the 'character' class.
	modPrediction <- rep(0, numInstances);
	for (i in 1:numInstances) {
		maxVal <- prediction[i,1];
		maxIndex <- 1;
		for (j in 1:numClasses) {
			if (prediction[i,j] > maxVal) {
				maxVal <- prediction[i,j];
				maxIndex <- j;
			}	
		}	
		modPrediction[i] <- colnames(prediction)[maxIndex];		
	}
	return(modPrediction)
}

library(foreign);
library(rpart);
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
	executionTime <- system.time(fit <- rpart(class ~ ., data = training, method = "class", control = rpart.control(minsplit=1)));
	
	#predictions on the test set
	prediction <- predict(fit, test[,-dim(test)[2]]);
	
	#'prediction' contain probability values for each of the classes.
	modPrediction <- findClass(prediction)   
	tab <- table(pred = modPrediction, true = as.character(test[,dim(test)[2]]));
	
	#write the results in the output file
	write.table(tab, file = outputFile, append = TRUE);
	cat(c("\nExecution Time:",executionTime[3], "\n\n"), file = outputFile, append = TRUE);
	
	# plot the decision tree
	#plot(fit, uniform=TRUE, main="Decision Tree - Class?");
	#text(fit, use.n=TRUE, all=TRUE, cex=.8);
}
}
