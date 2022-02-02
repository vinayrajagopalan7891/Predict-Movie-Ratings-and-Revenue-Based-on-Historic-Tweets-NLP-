#Initializing required libraries
if(!require("httr")) install.packages("httr"); library("httr")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")
if(!require("twitteR")) install.packages("twitteR"); library("twitteR")
library(stringr)
library(jsonlite)
library(curl)

#Read the files to initialise movie and release date
movies <- read_csv("./Group_Project/Data/movie_db.csv")
movies_dates <- read_csv("./Group_Project/Data/movie_imdb_db.csv")

tweet_details <- data.frame()

#For movie in movies fetch screen name
for(movie in movies$movie_names){
  url_for_id <- modify_url(
    url = "https://api.twitter.com",
    path = c("1.1", "users", 'search.json'),
    query = list(q= movie,
                 max_results = 100)
  )
  print(url_for_id)
  resUser <- GET(url = url_for_id,add_headers(authorization = paste0("Bearer ",BearerToken)))
  userid<- fromJSON(httr::content(resUser, "text"))
  
  if(length(userid)!=0){
    if(userid[1,'verified']) {
      userid$moviename <- movie
      tweet_details <- rbind(tweet_details, userid[1, c('id', "id_str", "name", "screen_name", "verified", 'moviename')])
    }
  }
}

#Initialise the screen names
movie_twitter_screenname <- tweet_details
movie_twitter_screenname_movie_imdb <- merge(x=movies, y=movie_twitter_screenname,by.x = 'movie_names',
                                             by.y='moviename', all.x = TRUE )
movie_twitter_screenname_movie_imdb$hashtag_movie <- str_replace_all(movie_twitter_screenname_movie_imdb$movie_names,
                                                                     "[[:space:]]", "")
movie_twitter_screenname_movie_imdb$hashtag_movie <- paste('#',movie_twitter_screenname_movie_imdb$hashtag_movie,sep = '')



####################################################################################
#Release dates with the movie IDs
movie_ids <- list(movies$movie_imdb_id)
class(movie_ids)
Movie_release_dates <- list()



#Iterate through the movie to fetch the release dates for the movie
for (id in movie_ids[[1]]){
  get_imdb_movie_get_meta_data_url <- modify_url(
    url = "https://imdb8.p.rapidapi.com/title/get-meta-data",
    query = list(
      ids = id
    ))
  movie_meta_data_res <- GET(url = get_imdb_movie_get_meta_data_url,add_headers(.headers = c('x-rapidapi-host' = 'imdb8.p.rapidapi.com','x-rapidapi-key' = 'cd83355a26msh4cad90d930a467dp16d45fjsn740267ab67a3')))
  movie_meta_data <- fromJSON(httr::content(movie_meta_data_res, "text"))
  Movie_release_dates <- append(Movie_release_dates,movie_meta_data)
}

movie_release_dates_list <- c()
movie_ids_new <- c()
for (i in 1:length(movie_ids[[1]])){
  movie_ids_new <-append(movie_ids_new,movie_ids[[1]][i])
  movie_release_dates_list <- append(movie_release_dates_list,Movie_release_dates[[i]][[4]])
}

movie_release_dates_df <- data.frame(movie_ids_new,movie_release_dates_list)
###############################################################################
movie_screenname_release_date <- merge(x=movie_twitter_screenname_movie_imdb, y=movie_release_dates_df,
                                       by.x = 'movie_imdb_id', by.y = 'movie_ids_new')

movie_screenname_release_date$movie_release_dates_list <- as.Date(movie_screenname_release_date$movie_release_dates_list)
date <- as.Date('2020-12-31')
movie_screenname_release_date <- movie_screenname_release_date %>%
  filter(movie_release_dates_list>date)

movie_screenname_release_date <- distinct(movie_screenname_release_date, movie_imdb_id, .keep_all = TRUE)

write.csv(movie_screenname_release_date,"./Group_Project/Data/movie_screenname_release_date.csv", row.names = FALSE)

###############################################################################
#Get Historical data using Full Archive search using dev environment
#Full Archive search has 50 request per user
#We plugged multiple bearer token to fetch the data
library('lubridate')
movie_screenname_release_date$toDate <- gsub("[: -]", "" , movie_screenname_release_date$movie_release_dates_list, perl=TRUE)
movie_screenname_release_date$toDate <- paste(movie_screenname_release_date$toDate,'0000',sep = '')
movie_screenname_release_date$fromDate <- gsub("[: -]", "" ,
                                               as.character(as.Date(movie_screenname_release_date$movie_release_dates_list)-30),
                                               perl=TRUE)
movie_screenname_release_date$fromDate <- paste(movie_screenname_release_date$fromDate,'0000',sep = '')
df_historical_tweets_5_2 <- data.frame()
df_1 <- data.frame()
batch_1 <- movie_screenname_release_date[1:50,]
batch_2 <- movie_screenname_release_date[51:100,]
batch_3 <- movie_screenname_release_date[101:150,]
batch_4 <- movie_screenname_release_date[151:200,]
batch_5 <- movie_screenname_release_date[201:250,]
batch_6 <- movie_screenname_release_date[251:300,]
batch_7 <- movie_screenname_release_date[301:350,]

for (i in 1:50){
  
  url_for_tweets <- modify_url(
    url = "https://api.twitter.com/1.1/tweets/search/fullarchive/TwitterSentiAnalysis.json",
    query= list(
      query = paste(batch_5[i,'hashtag_movie'], "lang:en - is"),
      fromDate = batch_5[i,'fromDate'],
      toDate = batch_5[i,'toDate']
    )
  )
  print(url_for_tweets)
  resUser <- GET(url = url_for_tweets,add_headers(authorization = paste0("Bearer ",'AAAAAAAAAAAAAAAAAAAAACi4YgEAAAAAnlZ7Fo6%2FB2ynY3olp1cYQZhyClg%3DgJlsjQAG0DXEM45rC9q5HKvCiCX5iTXpdoTwi9cYqAHCbvk7Dj')))
  userid<- fromJSON(httr::content(resUser, "text"))
  
  if(length(userid$results)>2){
    df_1 <- cbind(batch_5[i,'movie_imdb_id'],userid$results[,c("created_at","id","id_str","text","source","truncated",
                                                               "in_reply_to_status_id", "in_reply_to_status_id_str",
                                                               "in_reply_to_user_id" ,"in_reply_to_user_id_str",
                                                               "in_reply_to_screen_name","geo",
                                                               "coordinates", "contributors", "is_quote_status" ,"quote_count",
                                                               "reply_count", "retweet_count","favorite_count","favorited",
                                                               "retweeted", "possibly_sensitive", "filter_level",
                                                               "lang" )] )
  }
  if(ncol(df)==25){
    df_historical_tweets_5_2 <- rbind(df_historical_tweets_5_2,df_1)
  }
}
################################################################################
#Append all the data data frames of all the batches and write to .rds file
names <- c("IMDB_Movie_ID", "created_at","id","id_str","text","source","truncated",
           "in_reply_to_status_id", "in_reply_to_status_id_str",
           "in_reply_to_user_id" ,"in_reply_to_user_id_str",
           "in_reply_to_screen_name","geo",
           "coordinates", "contributors", "is_quote_status" ,"quote_count",
           "reply_count", "retweet_count","favorite_count","favorited",
           "retweeted", "possibly_sensitive", "filter_level",
           "lang" )

setnames(df_historical_tweets, names)
setnames(df_historical_tweets_2, names)
setnames(df_historical_tweets_2_1, names)
setnames(df_historical_tweets_2_2, names)
setnames(df_historical_tweets_3, names)
setnames(df_historical_tweets_4, names)
setnames(df_historical_tweets_5, names)
setnames(df_historical_tweets_5_1, names)
setnames(df_historical_tweets_5_2, names)
setnames(df_historical_tweets_6, names)
setnames(df_historical_tweets_7,names)

historical_data_df <- data.frame()

historical_data_df <- rbind(df_historical_tweets,df_historical_tweets_2,df_historical_tweets_2_1,df_historical_tweets_2_2,
                            df_historical_tweets_3,df_historical_tweets_4, df_historical_tweets_5, df_historical_tweets_5_1,
                            df_historical_tweets_5_2, df_historical_tweets_6, df_historical_tweets_7)

#Save the data as RDS
saveRDS(historical_data_df, file = "./Group_Project/Data/historical_data_df.rds")

