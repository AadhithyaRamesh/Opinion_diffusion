library(igraph)
library(hashmap)

#Work in progress

cluster_wise_user_analysis = function()
{
  cluster_table = read.csv("../Data/demonetization-tweets_Cluster_Tweet_Info.csv", stringsAsFactors = FALSE)
  tweet_info = read.csv("../Data/demonetization-tweets.csv", stringsAsFactors = FALSE)
  
  dominance = read.csv("../Data/Vertice_dataframe_demonet.csv", stringsAsFactors = FALSE)
  # print(head(dominance))

  
  unique_clusters = 1:max(cluster_table$Cluster_no)
  
  # Create new cluster table
  cluster_table$Created = NA
  cluster_table$User_name = NA
  cluster_table$Retweet_count = NA
  
  for (index in 1:nrow(cluster_table))
  {
    cluster_table$Created[index] = tweet_info$created[cluster_table$Tweet_ID[index]]
    cluster_table$User_name[index] = tweet_info$screenName[cluster_table$Tweet_ID[index]]
    cluster_table$Retweet_count[index] = tweet_info$retweetCount[cluster_table$Tweet_ID[index]]
    cluster_table$Dominance[index] = dominance[which(dominance$User_name == cluster_table$User_name[index]), ]$dominance[1]
    
    tweet_text = tweet_info$text[cluster_table$Tweet_ID[index]]
    
    # Split words of text
    tweet_words = strsplit(tweet_text, " ")[[1]]
    
    # check if tweet is a retweet
    if (tweet_words[1] == "RT")
    {
      # Remove @ and :
      original_user = substr(tweet_words[2],2,nchar(tweet_words[2])-1)
      cluster_table$Original_User[index] = original_user
    }
    else
    {
      cluster_table$Original_User[index] = cluster_table$User_name[index]
    }
    
    
    
  }
  
  for (index in 1:max(cluster_table$Cluster_no))
  {
    
    cluster = cluster_table[cluster_table$Cluster_no == index, ]

    write.csv(cluster, paste("../Results/", as.character(index), "_results.csv"))
    
    sorted_by_Created = cluster[order(cluster$Created), ]
    
    print(head(sorted_by_Created))
    
    sorted_by_retweet = cluster[order(-cluster$Retweet_count), ]
    
    
  }

}





cluster_wise_user_analysis()




