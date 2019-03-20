library(igraph)
library(hashmap)

retweet_table = read.csv("../Data/demonetization_retweet_table.csv", sep = ";", stringsAsFactors = FALSE)
cluster_table = read.csv("../Data/demonetization_cluster_tweet_table.csv", sep = ";", stringsAsFactors = FALSE)


unique_clusters = 1:max(cluster_table$Cluster.number)
cluster_tweet_indices = data.frame(Cluster.number = unique_clusters, tweet_indices = integer(length(unique_clusters)))

for (index in 1:nrow(cluster_table))
{
  
  data = as.list(cluster_table[index, ])
  
  tweet = data$tweet
  cluster_number = data$Cluster.number
  
  valid_indices = which(retweet_table$tweet == tweet)
  if (cluster_tweet_indices[cluster_number, ]$tweet_indices == 0)
  {
    if (length(valid_indices) == 0)
    {
      print(tweet)
    }
    cluster_tweet_indices$cluster_number[2] = valid_indices
  }
  else
  {
    cluster_tweet_indices[cluster_number, ]$tweet_indices = c(cluster_tweet_indices[cluster_number, ]$tweet_indices, valid_indices)
  }
  
}

print(cluster_tweet_indices)