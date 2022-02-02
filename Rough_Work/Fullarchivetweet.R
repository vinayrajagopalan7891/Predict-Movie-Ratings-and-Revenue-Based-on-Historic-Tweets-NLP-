library(rtweet)

tweet <- search_tweets(q="#NORAIn", token = twitter_token)

tweets <- rtweet::search_fullarchive(
  q = '#BlackWidow',
  n = 100,
  fromDate = '20200213',
  toDate = '20200313',
  parse = TRUE,
  env_name = 'Staging',
  token = twitter_token
)
rt <- search_fullarchive("#rstats", n = 300, env_name = "Staging",
                         fromDate = "201401010000", toDate = "201401312359")



url_for_id <- modify_url(
  url = "https://api.twitter.com/1.1/tweets/search/fullarchive/devenv.json",
  query= list(
    query = '#Superman',
    maxResults = 40,
    fromDate = '202006220000',
    toDate = '202007200000'
  )
)
resUser <- GET(url = url_for_id,add_headers(authorization = paste0("Bearer ",'AAAAAAAAAAAAAAAAAAAAADegYgEAAAAADZjQRNzsQZK2lPdT4oO2gtnx6Fs%3DmaTDLa0z0dtMGzK3XlF0BLyE97PIhJ8FRLePd4ZZGSEcoH212L')))
userid<- fromJSON(httr::content(resUser, "text"))
df <- as.data.frame(userid$results)
View(df)


twitter_token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_secret)

twitter_token

twitter_details <- read.csv("C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Social Media Analytics/Group_Project/movie_twitter_screenname_movie_imdb.csv")
movie_ids <- twitter_details$movie_imdb_id
screen_names <- twitter_details$screen_name
screen_name_tweets <- readRDS("C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Social Media Analytics/Group_Project/screen_name_tweets.rds")

for(index in 139:length(screen_names)) {
  screen_name <- screen_names[index]
  movie_id <- movie_ids[index]
  print(screen_name)
  tweets <- rtweet::get_timelines(screen_name,
                                  n = 5000,
                                  language = 'en',
                                  since = '2020-01-01',
                                  until = '2021-12-31',
                                  exclude_replies = FALSE,
                                  include_entities = TRUE, 
                                  token = twitter_token)
  screen_names_temp <- data.frame(movie_id, screen_name, tweets)
  screen_name_tweets <- rbind(screen_name_tweets, screen_names_temp)
}
View(screen_name_tweets)
saveRDS(screen_name_tweets, file = "C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Social Media Analytics/Group_Project/screen_name_tweets.rds")
write.csv(screen_name_tweets,"C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Social Media Analytics/Group_Project/screen_name_tweets_db.csv", row.names = FALSE)
