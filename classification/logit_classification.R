# Multinomial logistic regression  
{
	library(foreign);
	library(mlogit);
	try(dataFile <-scan(what = character(), nmax = 1), silent = FALSE); # take the input data set
	dataSet <- read.arff(dataFile);
	try(file <-scan(what = character(), nmax = 1), silent = FALSE); # take the name of the output file.
	try(numTrials <-scan(what = integer(), nmax = 1), silent = FALSE); # the number of trials.
	precision <- 0;
	recall <- 0;
	fmeasure <- 0;
	executionTime <- 0;
	split <- round(.34*dim(dataSet)[1]);
	for (j in 1:numTrials) {
		idx <- sample(1:dim(dataSet)[1], split);
		training <- dataSet[-idx,];
		test <- dataSet[idx,];
	
		# prepare data for the mlogit function
		# convert the categorical outcome variable as factor levels.

		mlogit.training <- mlogit.data(training, shape = "wide", choice = "class");
		executionTime <- executionTime + system.time(mlogit.r <- mlogit(class ~ 0 | ., data = mlogit.training))[3];		
		# We need to make the predictions
			

		#executionTime <- executionTime + system.time(logit.r <- glm(class ~ ., family = binomial("logit"), data = training))[3];
		#predictions <- predict(logit.r, test, type = "response");
		#predictions <- round(predictions); 	# round the values.

		# We need to calculate Time Taken, Precision, Recall, F-Measure, ROC Area, Kappa Stat.
		
		fp <- 0;	# of false positives. 
		fn <- 0;	# of false negatives 
		tp <- 0;	# of true positives
		tn <- 0;	# of true negatives
		
		actuals <- as.numeric(test[,dim(test)[2]]);
		for (i in 1:length(predictions)) {
			if ((predictions[i]+1 == actuals[i]) && actuals[i] == 2) {
				tp <- tp + 1; }
		
			if ((predictions[i]+1 == actuals[i]) && actuals[i] == 1) {
				tn <- tn + 1; }
		
			if ((predictions[i]+1 != actuals[i]) && actuals[i] == 2) {
				fn <- fn + 1; }
		
			if ((predictions[i]+1 != actuals[i]) && actuals[i] == 1) {
				fp <- fp + 1; }
		}
		
		precision <- precision + tp/(tp+fp);
		recall <- recall + tp/(tp+fn);
		fmeasure <- fmeasure + (2*tp)/(2*tp+fp+fn);
	}
	precision <- precision/numTrials;
	recall <- recall/numTrials;
	fmeasure <- fmeasure/numTrials;
	executionTime <- executionTime/numTrials;

	res <- c("precision", precision, "recall", recall, "fmeasure", fmeasure, "execution_time", executionTime[3]);
	write(res, file, ncol = 1);	# write the output file.
}
