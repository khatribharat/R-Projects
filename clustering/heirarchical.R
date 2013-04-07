# Heirarchical clustering
{
library(foreign);
library(cluster);
print("Usage: <source.R> <dataFile> <output file> <number of clusters> <cluster method = \"hclust\" or \"agnes\">");
try(dataFile <- scan(what = character(), nmax = 1), silent = FALSE);
try(output <- scan(what = character(), nmax = 1), silent = FALSE);
try(n <- scan(what = integer(), nmax = 1), silent = FALSE);
try(cluster.method <- scan(what = character(), nmax = 1), silent = FALSE);
dataSet <- read.arff(dataFile);
dataSet <- dataSet[,-dim(dataSet)[2]];

if (cluster.method == "hclust") {
	dataSet.cluster <- dist(dataSet, method = "euclidean");
	executionTime <- system.time(fit <- hclust(dataSet.cluster, method = "ward"));
	groups <- cutree(fit, k=n);
	cat(groups, executionTime[3], file = output, append = TRUE, sep = "\n");
}
else if (cluster.method == "agnes")
{
	executionTime <- system.time(fit <- agnes(dataSet, diss = FALSE, metric = "euclidean", stand = FALSE, method = "average"));
}

}
