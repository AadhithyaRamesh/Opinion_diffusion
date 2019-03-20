
library(hashmap)
library(igraph)


data = read.csv("../Data/demonetization-tweets.csv")
# data = head(data)
# str(data)
# print(head(data))

create_network = function(selected_tweets)
{
  # Create a dataframe nodes containing unique screenName
  nodes = data.frame(User_name = character(), User_type = character())
  
  # Create Edge List vectors
  retweeters = c()
  tweeters = c()
  edge_type = c()
  cluster_number = c()

  #Iterate over all selected tweets
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
    
  }
  
  # Create Edge List dataframe
  edges = data.frame(tweeters, retweeters, edge_type, stringsAsFactors = FALSE)
  
  # Combines repetitive edges to form a simgle edge with a weight attribute
  edges = aggregate(edges[,3], list(retweeters = edges[,1], tweeters = edges[,2], edge_type = edges[,3]), FUN = length)
  
  # Assign name to the created Weight column
  colnames(edges)[4] = "weight"

  # Convert edges and nodes dataframes to an igraph object
  network = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  # Finds cliques in the given network
  # cliques = cliques(network)

  print("Network Creation Done")
  
  # Make a very basic plot of the network
  # plot(network)
  
  # Finds betweenness centrality of each node in the network
  # print(betweenness(network))
  
  # Calls the dominance function.
  nodes = dominance(network, nodes)

  # Eliminate nodes with NA values  
  nodes[is.na(nodes)] = 0
  
  # Convert dominance inclusive dataframe to igraph object 
  network = graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  
  print("Dominance calculation done")
  
  # dominance_analysis(nodes, edges)

  # Writes the edges and nodes dataframes into CSV files
  write.csv(nodes, "../Data/Vertice_dataframe_demonet.csv")
  write.csv(edges, "../Data/Edge_dataframe_demonet.csv")
  
  print("Write done")

}

start.time <- Sys.time()

# For search keyword applied

# results = search_tweets_for_keyword("#Demonetization", data)
# # print("Results done")
# create_network(results)

#For raw data

create_network(data)


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
