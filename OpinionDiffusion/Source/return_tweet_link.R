return_tweet_link = function(tweet_words)
{
  for (word in tweet_words)
  {
    if (grepl("https", word))
    {
      return(word)
    }
  }
}