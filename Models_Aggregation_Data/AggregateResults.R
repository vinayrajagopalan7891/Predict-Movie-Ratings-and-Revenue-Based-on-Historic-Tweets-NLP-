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

#Initializing all the IMDB Files
movie_actor_last5_movies_df <- read.csv("./Group_Project/Data/movie_actor_last5_movies_db.csv", header=TRUE)
movie_last5_movie_df <- read.csv("./Group_Project/Data/movie_last5_movie_db.csv", header=TRUE)
actor_df <- read.csv("./Group_Project/Data/actor_db.csv", header=TRUE)
movie_imdb_df <- read.csv("./Group_Project/Data/movie_imdb_db.csv", header=TRUE)
names <- c('movie_imdb_id', 'revenue', 'total_runtime_in_minute', 'meta_critics_count','meta_score','release_date')
setnames(movie_imdb_df, names)

#Merging tables for aggregating
final_df <- merge(x = movie_actor_last5_movies_df, y = movie_last5_movie_df, by.x = "film_ids.index.", by.y = "movie_id", all.x = TRUE)
final_df_with_actor_stats <- merge(x = final_df, y = actor_df, by.x = "actor_id", by.y = "actor_id", all.x = TRUE)
names <- c('actor_id', 'last_5_movie_id', 'movie_imdb_id', 'actor_movie', 'actor_movie_year', 'revenue', 'total_runtime_in_minute', 'meta_critics_count','meta_score','release_date', 'actor_credits')
setnames(final_df_with_actor_stats, names)

final_df_with_actor_stats$actor_movie_year <- as.numeric(gsub("[^0-9]", "",final_df_with_actor_stats$actor_movie_year))
final_df_with_actor_stats$release_date <- as_datetime(final_df_with_actor_stats$release_date)
#Aggregating Results
agg_result <- final_df_with_actor_stats %>% group_by(movie_imdb_id) %>% 
                summarise(cast_last5_movies_avg_revenue = floor(mean(revenue, na.rm=TRUE)), 
                          cast_last5_movies_avg_total_runtime = floor(mean(total_runtime_in_minute, na.rm=TRUE)),
                          cast_last5_movies_avg_meta_critics_count = floor(mean(meta_critics_count, na.rm=TRUE)),
                          cast_last5_movies_avg_meta_score = floor(mean(meta_score, na.rm=TRUE)),
                          cast_last5_movies_avg_actor_credits = floor(mean(actor_credits, na.rm=TRUE)),
                          cast_last5_movies_max_release_date = max(release_date, na.rm=TRUE))


movie_df_final <- merge(x = movie_imdb_df, y = agg_result, by.x = "movie_imdb_id", by.y = "movie_imdb_id", all.x = TRUE)
#Writing the files after aggregation

write.csv(movie_df_final,"./Group_Project/Data/movie_db_final.csv", row.names = FALSE)
