# k-means clustering
{
library(foreign);
print("Usage: <source.R> <data file> <output file> <number of clusters>")
try(dataFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(file <- scan(what = character(), nmax = 1), silent = FALSE);
try(k <- scan(what = integer(), nmax = 1), silent = FALSE);
try(output <- scan(what = character(), nmax = 1), silent = FALSE);
dataSet <- read.arff(dataFile);
dataSet <- dataSet[,-dim(dataSet)[2]];
executionTime <- system.time(fit <- kmeans(dataSet, k));
cat(fit, executionTime[3], file = output, append = TRUE, sep = "\n");
}
