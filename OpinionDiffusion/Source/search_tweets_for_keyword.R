

data = read.csv("../Data/inauguration.csv")

#Function to search for input: keyword through input : tweets dataframe and gives output: List of Tweet indices whose
#text contain input: keyword
search_tweets_for_keyword = function(keyword, tweets)
{
  selected_tweets = data.frame()
  for (index in 1:nrow(tweets))
  {
    keyword = tolower(keyword)
    #Convert Dataframe row to named list.
    tweet = as.list(tweets[index,])
    
    tweet_text = tolower(as.character(tweet[["text"]])[[1]])
    
    # Search through text for keyword preceded by whitespace and followed by whitespace or punctuation
    if (grepl(paste("\\s", keyword,"(\\s|[.!,?])", sep = ""), tweet_text))
    {
          selected_tweets = rbind(selected_tweets, tweets[index,])
    }
    
  }
  return(selected_tweets)
}

# results = search_tweets_for_keyword("#Demonetization", data)