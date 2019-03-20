
library(hashmap)
library(igraph)
library(lubridate)


data = read.csv("../Data/demonetization-tweets.csv")

# Creates network by order of created time of tweets. Stores 24 hour dated chunks of tweets.
create_timestamp_network = function(selected_tweets)
{
  # Create a dataframe nodes containing unique screenName
  nodes = data.frame(User_name = character(), User_type = character())
  
  # Create Edge List vectors
  retweeters = c()
  tweeters = c()
  edge_type = c()
  cluster_number = c()
  
  #Sort dataframe by timestamp
  selected_tweets = selected_tweets[order(selected_tweets$created),] 

  #Record latest timestamp
  last_timestamp = as.character(as.list(selected_tweets[1,])[["created"]])
  
  #Convert to datetime objects
  last_timestamp = parse_date_time(last_timestamp, orders = "dmy HM")

  #Convert to IST
  last_timestamp = with_tz(last_timestamp, tz = "Asia/Kolkata")
  
  for (index in 1:nrow(selected_tweets))
  {

    # Convert dataframe row into named list
    tweet_data = as.list(selected_tweets[index,])
    
    current_timestamp = as.character(tweet_data[["created"]])
    
    #Convert to datetime objects
    current_timestamp = parse_date_time(current_timestamp, orders = "dmy HM")
    
    #Convert to IST
    current_timestamp = with_tz(current_timestamp, tz = "Asia/Kolkata")

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
    # print(edges)
    # print(nodes)

    # Convert friends matrix to an igraph object
    network = graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

    # cliques = cliques(network)
    print("Done")

    # print(edge_attr(network))
    # # Make a very basic plot of the network
    # plot(network)

    #Do operations on a daily cycle
    
    if (current_timestamp - last_timestamp > (3600 * 24))
      
      #calculate dominance
      nodes_dominance = dominance(network, nodes)
      
      nodes[is.na(nodes)] = 0
      last_timestamp = current_timestamp
      
      # print(selected_tweets)
      tweets_till_date = selected_tweets[1:index, ]
      
      # Create dated tweet chunks
      write.csv(tweets_till_date, "../Data/Tweets_till_date_demonet.csv")

      foldername = paste("Demonetization", as.character(format(current_timestamp, "%d_%m_%Y")),sep = "_")
      final_clusters("demonetization", "../Data/demonetization-tweets", foldername, "weighted", 10000)

      print(nodes_dominance)
  }
  # Create Edge List dataframe
  edges = data.frame(retweeters, tweeters, edge_type, stringsAsFactors = FALSE)

  # Combines repetitive edges to form a simgle edge with a weight attribute
  edges = aggregate(edges[,3], list(retweeters = edges[,1], tweeters = edges[,2], edge_type = edges[,3]), FUN = length)

  # Assign name to the created Weight column
  colnames(edges)[4] = "weight"

  # Convert friends matrix to an igraph object
  network = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

  # Make a very basic plot of the network
  # plot(network)
}

start.time <- Sys.time()

# results = search_tweets_for_keyword("#Demonetization", data)
# # print("Results done")
create_network(data)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
