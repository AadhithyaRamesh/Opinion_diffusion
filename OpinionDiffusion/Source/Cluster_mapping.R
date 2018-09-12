library(hashmap)
library(igraph)


cluster_data = read.csv("../Data/demonetization-tweets_Clusters.csv")

return_tweet_cluster = function(tweet)
{
  index = which(cluster_data$tweet == tweet)
  return(cluster_data$cl_num[[index]])
}
  
# print(return_tweet_cluster("RT @rssurjewala: Critical question: Was PayTM informed about #Demonetization edict by PM? It's clearly fishy and requires full disclosure &amp;.
"))


# RT rssurjewala: Critical question: Was PayTM informed about #Demonetization edict by PM Its clearly fishy and requires full disclosure &amp; 
# RT @rssurjewala: Critical question: Was PayTM informed about #Demonetization edict by PM? It's clearly fishy and requires full disclosure &amp;
