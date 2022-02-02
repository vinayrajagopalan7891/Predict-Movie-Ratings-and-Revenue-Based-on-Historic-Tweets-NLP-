# Initializing Libraries

library(tidyverse)
library(rvest)
library(lubridate)
library("XML")
library("xml2")
library(stringr)
library(hash)

#Read the base table with movies to fetch the cast for the movie
base_movie_df <- read.csv("C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Social Media Analytics/Group_Project/movie_db.csv", header=TRUE)
movie_ids <- base_movie_df$movie_imdb_id
movie_actor_df <- data.frame()

#Iterate through the movie to fetch all the cast associated with the movie
for(movie_id in movie_ids) {
  imdb_url <- "https://www.imdb.com/title/"
  imdb_url <- paste(imdb_url, movie_id, sep="")
  page <- read_html(imdb_url)
  actor_movie_imdb <- data.frame()
  
  actor_imdb <- page %>% html_elements("a[class='StyledComponents__ActorName-sc-y9ygcu-1 ezTgkS']")
  actor_imdb_href <- actor_imdb %>% html_attr("href")
  
  actor_imdb_id <- c()
  for(actor in actor_imdb_href) {
    actor_imdb_href <- gsub("[?]", ":", actor)
    actor_imdb_href
    href <- str_split(actor_imdb_href, ":")
    href <- str_split(href[[1]][1], '/')
    actor_imdb_id <- append(actor_imdb_id, href[[1]][3])
  }
  
  actor_name <- actor_imdb %>% html_text()
  movie_actor_df_temp <- data.frame(movie_id, actor_imdb_id, actor_name)
  movie_actor_df_temp
  movie_actor_df <- rbind(movie_actor_df, movie_actor_df_temp)
}

#Write the data to the file
write.csv(movie_actor_df,"C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Social Media Analytics/Group_Project/movie_actor_db.csv", row.names = FALSE)
