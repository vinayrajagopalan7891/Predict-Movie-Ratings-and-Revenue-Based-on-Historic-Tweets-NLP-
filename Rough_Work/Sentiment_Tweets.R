library("rtweet")

source("Twitter_API_Token.R")

twitter_token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv=FALSE)

print5


rtweets <- rtweet::search_tweets(q = "#Tesla",
                                 n = 10,include_rts = FALSE,token = twitter_token)

head(rtweets)


rtweets1 <-  search_tweets(
  "Tesla Complaint",
  n = 100,
  include_rts = TRUE,
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = NULL,
  retryonratelimit = FALSE,
  verbose = TRUE,
  lang = "en",
  until = ymd('2021-12-07')
)

head(rtweets1)