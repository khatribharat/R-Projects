# Decision Tree
{
library(foreign);
library(rpart);
try(dataFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(output <- scan(what = character(), nmax = 1), silent = FALSE);
dataSet <- read.arff(dataFile);
split <- round(.34*dim(dataSet)[1]);
idx <- sample(1:dim(dataSet)[1], split);
training <- dataSet[-idx,];
test <- dataSet[idx,];
executionTime <- system.time(fit <- rpart(class ~ ., data = training, method = "class", control = rpart.control(minsplit=1)))[3];
cat(fit, executionTime, file = output, append = TRUE, sep = "\n");

#predictions on the test set

# plot the decision tree
plot(fit, uniform=TRUE, main="Decision Tree - Class?");
text(fit, use.n=TRUE, all=TRUE, cex=.8);
}
