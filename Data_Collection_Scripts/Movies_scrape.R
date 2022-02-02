# Initializing Libraries
library(tidyverse)
library(rvest)
library(lubridate)
library("XML")
library("xml2")
library(stringr)

# Fetch American Movies Released in 2021
url = "https://en.wikipedia.org/wiki/List_of_American_films_of_2021"
movie_html = read_html(url)
movie_names <- c()
production_company <- c()

#Iterate through the tables returned and append movie_name, production house and imdb_id
for(i in 4:7) {
  movie_table = movie_html %>% html_nodes(css = "table")%>%nth(i) %>% html_table(fill = TRUE)
  movie_names <- append(movie_names, movie_table$Title)
  production_company <- append(production_company,movie_table$`Production company`)
}


# Fetch imdb Ids for the movies released in 2021
movie_imdb_id = c() 
for(movie in movie_names) {
  get_imdb_movie_id_url <- modify_url(
    url = "https://imdb8.p.rapidapi.com/title/find",
    query = list(
      q = movie
    ))
  
  movie_id_res <- GET(url = get_imdb_movie_id_url,add_headers(.headers = c('x-rapidapi-host' =  'imdb8.p.rapidapi.com', 'x-rapidapi-key' =  rapid_api_key)))
  
  movie_ids <- fromJSON(httr::content(movie_id_res, "text"))
  movie_id <- movie_ids$results$id[1]
  
  movie_id <- str_split(movie_id, "/")
  movie_id <- movie_id[[1]]
  movie_imdb_id = append(movie_imdb_id, movie_id[3])
}

#Append all the columns to create the dataframe movie_df
movie_df <- data.frame(movie_names, production_company, movie_imdb_id)

write.csv(movie_df,"./Group_Project/Data/movie_db.csv", row.names = FALSE)
