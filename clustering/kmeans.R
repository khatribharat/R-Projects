# k-means clustering
{
library(foreign);
try(dataFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(file <- scan(what = character(), nmax = 1), silent = FALSE);
try(k <- scan(what = integer(), nmax = 1), silent = FALSE);
try(output <- scan(what = character(), nmax = 1), silent = FALSE);
dataSet <- read.arff(dataFile);
dataSet <- dataSet[,-dim(dataSet)[2]];
executionTime <- system.time(fit <- kmeans(dataSet, k))[3];
cat(fit, executionTime, file = output, append = TRUE, sep = "\n");
}
