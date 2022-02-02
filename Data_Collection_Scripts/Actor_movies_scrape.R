# Initializing Libraries
library(tidyverse)
library(rvest)
library(lubridate)
library("XML")
library("xml2")
library(stringr)
library(hash)

#Read the table movie_actor to fetch last 5 movies associated with actors of the movie
actor_movie_df <- read.csv("C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Social Media Analytics/Group_Project/movie_actor_db.csv", header=TRUE)

#Initializing the dataframe
movie_actor_df <- data.frame()
'%!in%' <- function(x,y)!('%in%'(x,y))

#Iterating through the actors to fetch 5 movies associated with the actor
for(i in 1:nrow(actor_movie_df)){
  print(i)
  actor_id <- actor_movie_df[i, 'actor_imdb_id']
  movie_id_check <- actor_movie_df[i, 'movie_id']
  actor_url <- "https://www.imdb.com/name/"
  actor_url <- paste(actor_url, paste(actor_id,'/#actor',sep=""), sep="")
  actor_url
  
  
  actor_page <- read_html(actor_url)
  actor_film_a <- actor_page %>% html_elements('div[class=filmo-category-section]') %>% html_elements('a') 
  actor_film_href <- actor_film_a%>% html_attr('href')
  actor_film_names <- actor_film_a%>% html_text()
  
  film_ids <- c()
  
  for(film_href in actor_film_href) {
    film_href <- gsub("[?]", ":", film_href)
    film_href <- str_split(film_href, ":")
    film_href <- str_split(film_href[[1]][1], '/')
    film_id <- film_href[[1]][3]
    film_ids <- append(film_ids, film_id)
  }
  
  index = match(movie_id_check, film_ids)
  counter = 1
  while(counter <= 5 && index < length(film_ids)){
    index <- index + 1
    actor_jump_to_div <- actor_page %>% html_elements('div[id=jumpto]')
    actor_suf <- actor_jump_to_div %>% html_elements('a')%>% html_text()
    
    div_id <- paste(paste(str_to_lower(actor_suf[1]),'-',sep=""),film_ids[index],sep="")
    actor_div_tag<-paste(paste('div[id=',div_id, sep=""),']',sep='')
    if(grepl("actor", actor_div_tag) || grepl("actress", actor_div_tag)) {
      actor_div <- actor_page %>% html_elements(actor_div_tag)
      movie_ignore <- actor_div %>% html_elements('div[class=filmo-episodes]')%>% html_elements('a') %>% html_text()
      actor_movie <- actor_div %>% html_elements('a') %>% html_text()
      actor_movie_year <- actor_div %>% html_elements("span[class='year_column']") %>% html_text()
      if(length(actor_movie) == 1 && actor_movie %!in% movie_ignore){
        counter <- counter + 1
        movie_actor_df_temp <- data.frame(movie_id_check, actor_id, film_ids[index], actor_movie, actor_movie_year)
        movie_actor_df <- rbind(movie_actor_df, movie_actor_df_temp)
      }
    }
  }
}

#Writing the data to the file
write.csv(movie_actor_df,"C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Social Media Analytics/Group_Project/movie_actor_last5_movies_db.csv", row.names = FALSE)
