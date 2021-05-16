
# Clear the workspace
rm(list=ls())
cat("\014")
library(ggplot2)
library(cluster)
# set working directory
setwd


df <- read.csv(file='AirlineLoyalty.csv', stringsAsFactors = FALSE)
summary(df)
df.norm <- df[, -c(1,12)]  
df.norm <- data.frame(sapply(df.norm, scale))

###### 2. Hierarchical Clustering ###### 
# 2.1 calculate the distance matrix & apply hclust()
euclidian_dist <- dist(df.norm, method = "euclidean")
hcluster.out <- hclust(euclidian_dist, method="ward.D")

# 2.2 plot the cluster
plot(hcluster.out)

# 2.3 cut the dendrogram into 3 clusters & append the cluster label to df
df$cluster_hc <- as.character(cutree(hcluster.out, k = 3))


###### 3. Kmeans Clustering ###### 
# for replication purpose
set.seed(123)

# 2.1 Choosing k using the "elbow plot" 
choosek <- data.frame(matrix(ncol = 2, nrow = 0)) 
colnames(choosek) <- c("numClusters", "avgWithinSS")
for (k in 2:10) {
  tempkm <- kmeans(df.norm, centers = k, nstart = 10)  
  tempdf <- data.frame(numClusters = k, avgWithinSS = mean(tempkm$withinss))
  choosek <- rbind(choosek, tempdf)
}

g <- ggplot(choosek, aes(numClusters, avgWithinSS))  
g + geom_line() + labs(x="Number of Clusters (k)", y="Average Within-Cluster Squared Distance")
rm(choosek, tempdf, tempkm)


###### 4. Compare Clustering Results and Label Clusters ###### 

km.out <- kmeans(df.norm, centers = 3, nstart = 10) 
# append the cluster label to df
df$cluster_km <- as.character(km.out$cluster)

# the number of observations in each cluster #
table(df$cluster_km)
table(df$cluster_hc)

# summary statistics of original features by cluster
df$cluster_hc <- as.factor(df$cluster_hc)
df$cluster_km <- as.factor(df$cluster_km)

# cluster_km #
agg_km <- aggregate(. ~ cluster_km, data=df[, -c(1, 13)], FUN = mean)
t(agg_km)

# cluster_hc #
agg_hc <- aggregate(. ~ cluster_hc, data=df[, -c(1, 14)], FUN = mean)
t(agg_hc)

# Interpretation: You can have multiple ways to define profile for customers belonging to each cluster. 
# Taking df$cluster_km for instance:
# cluster (Not-so-frequent Travelers): This cluster has the highest number of customers (2914 out of 4999) and they travel very infrequent.
# They are new customers (the smallest "Days_since_enroll") who have the least flight miles eligible for award travel (lowest "Balance").

# cluster (High Frequency Travelers): This cluster has the lowest number of customers (161 out of 4999) and these customers travel very frequently. 
# They have covered the maximum number flight miles (largest "Flight_miles_12mo") and made significantly more non-flight transactions in the past 12 months ("Flight_trans_12"). 
# They also have the maximum number of miles counted as qualifying for Top Flight Status (Qual_miles) and highest number of miles eligible for award travel ("Balance").

# cluster (Loyal Customers): This cluster consists 18.6% (23/3999) of the customers. 
# They are the "oldest" customers (largest "Days_since_enroll") who have relatively high flight miles eligible for award travel ("Balance"). 
# They use the frequent flyer & small business credit card the most often (highest "cc1_miles" & "cc3_miles")






