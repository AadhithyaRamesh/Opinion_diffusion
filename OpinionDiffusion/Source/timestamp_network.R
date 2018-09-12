
library(hashmap)
library(igraph)




data = read.csv("Data/demonetization-tweets.csv")
# data = head(data)
# str(data)

create_network = function(selected_tweets)
{
  # Create a dataframe nodes containing unique screenName
  nodes = data.frame(User_name = character(), User_type = character())
  
  # Create Edge List vectors
  retweeters = c()
  tweeters = c()
  edge_type = c()
  cluster_number = c()
  
  #Sort dataframe by timestamp
  print(selected_tweets)
  selected_tweets = selected_tweets[order(selected_tweets$created),] 
  print(selected_tweets)
  
  for (index in 1:nrow(selected_tweets))
  {
    
    # Convert dataframe row into named list
    tweet_data = as.list(selected_tweets[index,])
    
    # Lower-case screenName
    tweet_user = tolower(as.character(tweet_data[["screenName"]])[[1]])
    
    # Convert to character class
    tweet_text = as.character(tweet_data[["text"]])
    # text = c(text, tweet_text)    
    
    # Split words of text
    tweet_words = strsplit(tweet_text, " ")[[1]]
    
    #Add user ScreenName to Nodes if not present    
    if (!(tweet_user %in% nodes[,1]))
    {
      nodes = rbind(nodes, data.frame("User_name" = tweet_user, "Node_type" = "User"))
    }
    
    
    # cluster_number = c(cluster_number, return_tweet_cluster(tweet_text))
    
    
    # check if tweet is a retweet
    if (tweet_words[1] == "RT")
    {
      # Remove @ and :
      original_user = substr(tweet_words[2],2,nchar(tweet_words[2])-1)
      # tweet_link = return_tweet_link(tweet_words)
      # print(tweet_link)
      
      # Add original poster's screenName to Nodes if not present
      if (!(original_user %in% nodes[,1]))
      {
        nodes = rbind(nodes, data.frame("User_name" = original_user, "Node_type" = "User")) 
      }
      
      # Add original and retweet poster screenNames to Edge List
      tweeters = c(tweeters, original_user)
      retweeters = c(retweeters, tweet_user)
      edge_type = c(edge_type, "Tweet_edge")
      # edge_weight = c(edge_weight, 1)
    
      
    }

    # Create Edge List dataframe
    edges = data.frame(retweeters, tweeters, edge_type, stringsAsFactors = FALSE)
    edges = aggregate(edges[,3], list(retweeters = edges[,1], tweeters = edges[,2], edge_type = edges[,3]), FUN = length)
    
    colnames(edges)[4] = "weight"
    print(edges)
    print(nodes)
    
    # Convert friends matrix to an igraph object
    network = graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
    
    # cliques = cliques(network)
    print("Done")
    
    print(edge_attr(network))
    # Make a very basic plot of the network
    plot(network)
    
    readline(prompt = "Press any key to go to next iteration.")
  }
  # cbind(nodes, text)
  # Create Edge List dataframe
  edges = data.frame(retweeters, tweeters, edge_type, stringsAsFactors = FALSE)
  edges = aggregate(edges[,3], list(retweeters = edges[,1], tweeters = edges[,2], edge_type = edges[,3]), FUN = length)

  colnames(edges)[4] = "weight"
  print(edges)
  print(nodes)
  
  # Convert friends matrix to an igraph object
  network = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  # cliques = cliques(network)
  print("Done")
  
  print(edge_attr(network))
  # Make a very basic plot of the network
  plot(network)
    
  
}

start.time <- Sys.time()

results = search_tweets_for_keyword("#Demonetization", data)
# print("Results done")
create_network(results)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
