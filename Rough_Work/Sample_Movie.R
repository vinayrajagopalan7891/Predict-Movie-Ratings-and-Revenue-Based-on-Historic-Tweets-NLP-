library(tidyverse)
library(rvest)
library(lubridate)
library("XML")
library("xml2")
library(stringr)

#Scarping Movies Released in Last Quarter of 2021
url = "https://en.wikipedia.org/wiki/List_of_American_films_of_2021"
movie_html = read_html(url)
movie_table = 
  movie_html %>% 
  html_nodes(css = "table") %>% 
  nth(7) %>% 
  html_table(fill = TRUE)
movie_names <- movie_table$Title
movie_cast_crew <- movie_table$`Cast and crew`

movie_names[1]
movie_cast_crew[1]
movie_cast_df <- data.frame(movie_names, movie_cast_crew)
head(movie_cast_df)

# Cast for a movie 
director <- str_split(movie_cast_crew[1], '(director)')[[1]][1]
director <- gsub("[^a-zA-Z ]", "", director)
director

screenplay <- str_split(str_split(movie_cast_crew[1], '(screenplay)')[[1]][1], ',')
screenplay <- gsub("[^a-zA-Z ]", "", screenplay[[1]][2])
screenplay

cast <- str_split(movie_cast_crew[1], ";")[[1]][2]
cast <- str_split(cast, ',')[[1]]
cast


#Get IMDB movie id
get_imdb_movie_id_url <- modify_url(
  url = "https://imdb8.p.rapidapi.com/title/find",
  query = list(
    q = movie_names[1]
  ))

movie_id_res <- GET(url = get_imdb_movie_id_url,add_headers(.headers = c('x-rapidapi-host' =  'imdb8.p.rapidapi.com', 'x-rapidapi-key' =  'c152df57e4mshc3d4cda6d4a90a8p176431jsn1f7d2cac8de9')))

movie_ids <- fromJSON(httr::content(movie_id_res, "text"))
movie_id <- movie_ids$results$id[1]

movie_id <- str_split(movie_id, "/")
movie_id <- movie_id[[1]]
movie_id[3]

#Get IMDB movie meta data, will include ratings, metacritic, release date
get_imdb_movie_get_meta_data_url <- modify_url(
  url = "https://imdb8.p.rapidapi.com/title/get-meta-data",
  query = list(
    ids = movie_id[3]
  ))

movie_meta_data_res <- GET(url = get_imdb_movie_get_meta_data_url,add_headers(.headers = c('x-rapidapi-host' =  'imdb8.p.rapidapi.com', 'x-rapidapi-key' =  'c152df57e4mshc3d4cda6d4a90a8p176431jsn1f7d2cac8de9')))

movie_meta_data <- fromJSON(httr::content(movie_meta_data_res, "text"))
release_date <- ymd(movie_meta_data$tt7097896$releaseDate)
View(movie_meta_data)

#Get IMDB movie business data, will include opening weekened revenue and budget is optional
#movie might have a budget or there wont be any budget
get_imdb_movie_get_business_url <- modify_url(
  url = "https://imdb8.p.rapidapi.com/title/get-business",
  query = list(
    tconst = movie_id[3]
  ))

movie_business_data_res <- GET(url = get_imdb_movie_get_business_url,add_headers(.headers = c('x-rapidapi-host' =  'imdb8.p.rapidapi.com', 'x-rapidapi-key' =  'c152df57e4mshc3d4cda6d4a90a8p176431jsn1f7d2cac8de9')))

movie_business_data <- fromJSON(httr::content(movie_business_data_res, "text"))


#Get youtube video related to the movie
movie_names[1]
google_bearer_token = "AIzaSyCI5JWqwhwlM2dmpycQbiGpez9rBMrbxYg"
get_youtube_movie_trailer_id_url <- modify_url(
  url = "https://youtube.googleapis.com/youtube/v3/search",
  query = list(
    part='snippet',
    q= movie_names[1],
    key=google_bearer_token
  ))

movie_youtube_movie_trailer_res <- GET(url = get_youtube_movie_trailer_id_url)

youtubeContent <- fromJSON(httr::content(movie_youtube_movie_trailer_res,"text"))
youtube_movie_videos_id <- youtubeContent$items$id$videoId
youtube_movie_videos <- youtubeContent$items$snippet
youtube_movie_videos <- cbind(youtube_movie_videos_id, youtube_movie_videos)
youtube_movie_videos <- youtube_movie_videos[youtube_movie_videos$publishedAt > release_date, 'youtube_movie_videos_id']
youtube_movie_videos[2]
video_ids = youtube_movie_videos[1]
for(ele in 2:length(youtube_movie_videos)){
  video_ids = paste(video_ids, youtube_movie_videos[ele] , sep=',')
}
video_ids
# Get youtube stats for the videos
get_youtube_video_stat_url <- modify_url(
  url = "https://youtube.googleapis.com/youtube/v3/videos",
  path = c("youtube", "v3", "videos"),
  query = list(
    part= "statistics",
    id = video_ids,
    key=google_bearer_token
  )
)
get_youtube_video_stat_url
youtube_movie_videos[1]

youtube_video_stats_res <- GET(url = get_youtube_video_stat_url)

youtube_video_stats <- fromJSON(httr::content(youtube_video_stats_res,"text"))
youtube_video_stats <- youtube_video_stats$items$statistics
view(youtube_video_stats)

# Find last 5 movies for cast
imdb_url <- "https://www.imdb.com/title/"
imdb_url <- paste(imdb_url, movie_id[3], sep="")
imdb_url


page <- read_html(imdb_url)

span_list_item <- page %>% html_elements("li[class='ipc-metadata-list__item BoxOffice__MetaDataListItemBoxOffice-sc-40s2pl-2 gwNUHl']") 
span_list_item
revenue <- span_list_item%>%html_elements('span') %>% html_text()
index = which(revenue == "Opening weekend US & Canada")
index
revenue <- as.numeric(gsub("[^0-9]", "", revenue[index + 1]))
revenue


revenue_date <- as.Date(revenue[index + 2], format = '%b %d, %y')
revenue_date 

number_of_nominations <- span_list_item[1] %>% html_text()
number_of_nominations <- as.numeric(gsub("[^0-9]", "", number_of_nominations))
number_of_nominations

runtime_node <- page %>% html_elements("li[class='ipc-metadata-list__item']") 
runtime <- runtime_node[10] %>% html_text()
runtime <- gsub("hour", ":", runtime)
runtime <- gsub("[^0-9 :]", "", runtime)
runtime <- str_split(runtime,':')
runtime_hour <- as.numeric(runtime[[1]][1])
runtime_hour
runtime_minutes <- as.numeric(runtime[[1]][2])
runtime_minutes
total_runtime_in_minutes <- runtime_hour * 60 + runtime_minutes
total_runtime_in_minutes

meta_score <- as.numeric(page %>% html_elements("span[class='score-meta']") %>% html_text())
meta_score
meta_crtics_count <-page %>% html_elements("span[class='score']")
meta_crtics_count <- as.numeric(meta_crtics_count[2] %>%html_text())
meta_crtics_count

actor_imdb <- page %>% html_elements("a[class='StyledComponents__ActorName-sc-y9ygcu-1 ezTgkS']")
actor_imdb_href <- actor_imdb %>% html_attr("href")

actor_imdb_href[1] <- gsub("[?]", ":", actor_imdb_href[1])
actor_imdb_href[1]
href <- str_split(actor_imdb_href[1], ":")
href <- str_split(href[[1]][1], '/')
actor_id <- href[[1]][3]
actor_id
actor_name <- actor_imdb %>% html_text()
actor_name

director_imdb <- page%>% html_elements("a[class='ipc-metadata-list-item__list-content-item ipc-metadata-list-item__list-content-item--link']")
director_imdb_href <- director_imdb[1] %>% html_attr("href")
director_href <- str_split(director_imdb_href[1], ":")
director_href <- str_split(director_href[[1]][1], '/')
director_id <- director_href[[1]][3]
director_id
director_name <- director_imdb %>% html_text()
director_name[1]

screenplay_imdb_href <- director_imdb[2] %>% html_attr("href")
screenplay_href <- str_split(screenplay_imdb_href[1], ":")
screenplay_href <- str_split(screenplay_href[[1]][1], '/')
screenplay_id <- screenplay_href[[1]][3]
screenplay_id
screenplay_name <- director_imdb %>% html_text()
screenplay_name[2]

#Get Last 10 movies for actor, director and screenplay
actor_url <- "https://www.imdb.com/name/"
actor_url <- paste(actor_url, paste(actor_id,'/#actor',sep=""), sep="")
actor_url


actor_page <- read_html(actor_url)
actor_film_href <- actor_page %>% html_elements('div[class=filmo-category-section]') %>% html_elements('a') %>% html_attr('href')
actor_film_href

actor_film_href[1] <- gsub("[?]", ":", actor_film_href[1])
actor_film_href[1]
film_href <- str_split(actor_film_href[1], ":")
film_href <- str_split(film_href[[1]][1], '/')
film_id <- film_href[[1]][3]
film_id 

actor_jump_to_div <- actor_page %>% html_elements('div[id=jumpto]')
actor_suf <- actor_jump_to_div %>% html_elements('a')%>% html_text()

div_id <- paste(paste(str_to_lower(actor_suf[1]),'-',sep=""),film_id,sep="")
div_id
actor_div_tag<-paste(paste('div[id=',div_id, sep=""),']',sep='')
actor_div_tag

actor_div <- actor_page %>% html_elements(actor_div_tag)
actor_movie <- actor_div %>% html_elements('a') %>% html_text()

actor_movie

actor_movie_year <- actor_div %>% html_elements("span[class='year_column']") %>% html_text()
actor_movie_year 

number_of_credits <- actor_page%>% html_elements("div[class='head']")%>%html_text()
number_of_credits <- as.numeric(gsub("[^0-9]","",number_of_credits[1]))
number_of_credits 


