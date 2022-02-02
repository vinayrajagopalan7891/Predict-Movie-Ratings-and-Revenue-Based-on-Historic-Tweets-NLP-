
# load some packages that we will use
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}
library(udpipe)
library(tidyr)
library(dplyr)
library(tm)
library(tidytext)
library(textstem)
if (!require("Matrix")) install.packages("Matrix", quiet=TRUE) ; require("Matrix")
if (!require("irlba")) install.packages("irlba", quiet=TRUE) ; require("irlba")
if (!require("randomForest")) install.packages("randomForest", quiet=TRUE) ; require("randomForest")
require(neuralnet)
library(neuralnet)
library(caret)

#Movies DB
movie_db <- read.csv("./Group_Project/Data/movie_db.csv")
# all the Movies names
#IMDB DB
imdb_db <- read.csv("./Group_Project/Data/movie_imdb_db.csv")


#Intializing Variables
movie_names <- movie_db$movie_names
movie_ids <- movie_db$movie_imdb_id
prod_name <- movie_db$production_company
release_date <- imdb_db$release_dates
mindate <- '2018-01-07T16:21:08Z'
google_bearer_token = "AIzaSyDva2wby2e8xIdUnh_J5jnEowYY17U59nY"
movie_videos_df <- data.frame()

#Reading movies DB
movie_videos_df_filtered <- read.csv("./Group_Project/Data/movie_videos_db.csv", header=TRUE)


#Get youtube video related to the movies
channel_df = data.frame()

#Iterating through movies to fetch trailer videos for that movie 
for (movie in 1:length(movie_names)) {
  get_youtube_movie_trailer_id_url <- modify_url(
    url = "https://youtube.googleapis.com/youtube/v3/search",
    query = list(
      part='snippet',
      q= movie_names[movie],
      key=google_bearer_token
    ))
  
  youtube_movie_trailer <- GET(url = get_youtube_movie_trailer_id_url)
  youtubeContent <- fromJSON(httr::content(youtube_movie_trailer,"text"))
  movie_df_temp <- data.frame(youtubeContent$items$id$videoId, youtubeContent$items$snippet$publishedAt)
  movie_df_temp <- movie_df_temp[which(movie_df_temp$youtubeContent.items.snippet.publishedAt < release_date[movie] &
                                         movie_df_temp$youtubeContent.items.snippet.publishedAt > mindate),]
  if(nrow(movie_df_temp) > 0){
    movie_df_temp <- data.frame(movie_names[movie],movie_ids[movie], movie_df_temp)
    movie_videos_df_filtered <- rbind(movie_videos_df_filtered, movie_df_temp)
  }
  Sys.sleep(5)
}

#Writing the collected video_ids and movie_ids to file
write.csv(movie_videos_df_filtered,"./Group_Project/Data/movie_videos_db.csv", row.names = FALSE)




# Get youtube video statistics 
# Initializing not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#Columns to add
columns <- c("viewCount","likeCount","favoriteCount","commentCount")
video_ids <- movie_videos_df_filtered$youtubeContent.items.id.videoId
movie_ids <- movie_videos_df_filtered$movie_ids.movie.
#Looping through video ids to fetch the stats for the videos
for (id in 1:length(video_ids)){
  get_youtube_video_stat_url <- modify_url(
    url = "https://youtube.googleapis.com/youtube/v3/videos",
    path = c("youtube", "v3", "videos"),
    query = list(
      part= "statistics",
      id = video_ids[id],
      key=google_bearer_token
    )
  )
  
  youtube_video_stats_res <- GET(url = get_youtube_video_stat_url)
  youtube_video_stats <- fromJSON(httr::content(youtube_video_stats_res,"text"))
  youtube_video_stats <- youtube_video_stats$items$statistics
  stats_temp <- data.frame(movie_ids[id], video_ids[id],youtube_video_stats)
  
  columns_loop <- colnames(youtube_video_stats)
  #Append values if col in column_loop
  for(col in columns){
    
    if (col %!in% columns_loop) {
      youtube_video_stats[col] <- 0 
    }
    
  }
  if (ncol(stats_temp) > 5){
    stats_df <- rbind(stats_df,stats_temp)
  }
  
  Sys.sleep(5)
}


#Write video stats to file
write.csv(stats_df,"./Group_Project/Data/video_stats_db.csv", row.names = FALSE)



#Get Channel IDs of videos 
prod_name <- movie_db$production_company
prod_name <- str_split(prod_name,"/")
prod_name <- unique(unlist(prod_name)) 

#Writing production house to a file
write.csv(prod_name,"./Group_Project/Data/prod_names_db.csv", row.names = FALSE)
prod_name <- data.frame(prod_name)
channel_id_df2 <- data.frame()
for (i in 1:nrow(prod_name)){
  get_channel_id_url <- modify_url(
    url = "https://youtube.googleapis.com/youtube/v3/search",
    query = list(
      part='snippet',
      q= prod_name$prod_name[i],
      key=google_bearer_token
    ))
  get_channel_id <- GET(url = get_channel_id_url)
  youtubeContent <- fromJSON(httr::content(get_channel_id,"text"))
  if(!is.null(youtubeContent$items$id$channelId[1])){
    channel_id_temp <- data.frame(prod_name$prod_name[i],youtubeContent$items$id$channelId[1])
    if(length(youtubeContent$items$id$channelId[1]) > 0){
      channel_id_df2 <- rbind(channel_id_df2,channel_id_temp) 
    }
  }
}

#Write data to the file

write.csv(channel_id_df2,"./Group_Project/Data/channel_id_db.csv", row.names = FALSE)


channel_db <-  read.csv("./Group_Project/Data/channel_id_db.csv")

channel_db_temp = channel_db %>% distinct(prod_name.prod_name.i.)


channel_final <- distinct(channel_db, prod_name.prod_name.i. , .keep_all = TRUE)





##Channel Statistics by channel ID 

# channel_stats <- data.frame()
# #channel_id <- channel_final$youtubeContent.items.id.channelId.1.
# #channel_name <- channel_final$prod_name.prod_name.i.
# #channel_id <- data.frame(channel_id)
channel_stats <- data.frame()
channel_id <- channel_final$youtubeContent.items.id.channelId.1.

channel_id <- data.frame(channel_id)

channel_stats_final <- data.frame()
for (i in 1:173){
  if (is.na(channel_id$channel_id[i])){
    next
  }
  print(channel_id$channel_id[i])
  get_channel_stats_url <- modify_url(
    url = "https://youtube.googleapis.com/youtube/v3/channels",
    query = list(
      part='snippet,contentDetails,statistics',
      id= channel_id$channel_id[i],
      key=google_bearer_token
    ))
  get_channel_stats_url
  
  channel_stats <- GET(url = get_channel_stats_url)
  youtube_stats_Content <- fromJSON(httr::content(channel_stats,"text"))
  channel_stats_temp <- data.frame(channel_id$channel_id[i],youtube_stats_Content$items$statistics)
  print("binded")
  if (ncol(channel_stats_temp) > 4){
    channel_stats_final <- rbind(channel_stats_final,channel_stats_temp)
  }
  
}
write.csv(channel_stats_final,"./Group_Project/Data/channel_stats_db.csv", row.names = FALSE)


channel_final # channelid 
channel_stats_final <- read.csv("./Group_Project/Data/channel_stats_db.csv")



#Merging channel data
channel_stats_db <- read.csv("./Group_Project/Data/channel_stats_db.csv")
channel_id <- read.csv("./Group_Project/Data/channel_id_db.csv")


channel_df_final <- merge(x =channel_id,y= channel_stats_db,by.x ='youtubeContent.items.id.channelId.1.',by.y='channel_id.channel_id.i.')
nrow(channel_df_final)


channel_stats_db_distinct <- distinct(channel_stats_db, channel_id.channel_id.i. , .keep_all = TRUE)
channel_id_distinct <-  distinct(channel_id, youtubeContent.items.id.channelId.1. , .keep_all = TRUE)
channel_df_final <- merge(x = channel_id_distinct,y = channel_stats_db_distinct,by.x ='youtubeContent.items.id.channelId.1.',by.y='channel_id.channel_id.i.', all.x = TRUE)


write.csv(channel_df_final,"./Group_Project/Data/channel_df_final.csv")


#------------------------- 
#Grouping Video Stats :
video_stats_db <- read.csv("./Group_Project/Data/video_stats_db.csv")

video_df_grouped <- video_stats_db %>% group_by(movie_ids.id.) %>%
  summarise(viewCount=mean(viewCount),
            likeCount=mean(likeCount),
            commentCount = mean(commentCount))
#------------------------- 


#------------------------- 
#Grouping Movie by date taking max:
movie_videos_db <- read.csv("./Group_Project/Data/movie_videos_db.csv")

movie_video_grouped <- movie_videos_db %>% group_by(movie_ids.movie.) %>%
  summarise(youtubeContent.items.snippet.publishedAt=max(youtubeContent.items.snippet.publishedAt))

write.csv(movie_video_grouped,"./Group_Project/Data/movie_df_grouped.csv")
#-------------------------          


#-------------------------
#Video df Final

video_df_grouped
movie_video_grouped

options(scipen = 999)
video_df_final <- merge(x = movie_video_grouped, y= video_df_grouped, by.x= 'movie_ids.movie.', by.y = 'movie_ids.id.',all.x = TRUE )

video_df_final %>% 
  mutate_if(is.numeric, round)

write.csv(video_df_final,"./Group_Project/Data/video_df_final.csv")
#-------------------------



#-------------------------

#Merging movie_db ane video_db 

movie_db
video_df_final


movie_video_merged <- merge(x= movie_db , y= video_df_final, by.x = 'movie_imdb_id',by.y = 'movie_ids.movie.',all.x = TRUE)


twitter_data <- readRDS("./Group_Project/Data/historical_data_df.rds")
