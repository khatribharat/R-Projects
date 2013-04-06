# Heirarchical clustering
{
library(foreign);
library(cluster);
try(dataFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(file <- scan(what = character(), nmax = 1), silent = FALSE);
try(n <- scan(what = integer(), nmax = 1), silent = FALSE);
try(output <- scan(what = character(), nmax = 1), silent = FALSE);
try(cluster.method <- scan(what = character(), nmax = 1), silent = FALSE);
dataSet <- read.arff(dataFile);
dataSet <- dataSet[,-dim(dataSet)[2]];

if (cluster.method == "hclust") {
	dataSet.cluster <- dist(dataSet, method = "euclidean");
	executionTime <- system.time(fit <- hclust(dataSet.cluster, method = "ward"))[3];
	groups <- cutree(fit, k=n);
	cat(groups, executionTime, file = output, append = TRUE, sep = "\n");
}
else if (cluster.method == "agnes")
{
	executionTime <- system.time(fit <- agnes(dataSet, diss = FALSE, metric = "euclidean", stand = FALSE, method = "average"));
}

}
