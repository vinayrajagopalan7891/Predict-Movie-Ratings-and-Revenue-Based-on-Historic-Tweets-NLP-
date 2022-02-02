# Initializing Libraries
library(tidyverse)
library(rvest)
library(lubridate)
library("XML")
library("xml2")
library(stringr)

#Reading the file to initialize pre existing data
movie_actor_last5_movies_df <- read.csv("./Group_Project/Data/movie_actor_last5_movies_db.csv", header=TRUE)
movie_ids <- unique(movie_actor_last5_movies_df$film_ids.index.)
movie_last5_movie_df <- read.csv("./Group_Project/Data/movie_last5_movie_db.csv", header=TRUE)
index <-  match(movie_ids[nrow(movie_last5_movie_df)], movie_ids)
index
index <- index  + 1

# Iterate through each movie and fetch data associated with the movie
for(i in index:13000) {
  if(!is.na(movie_ids[index])){
    movie_id <- movie_ids[i]
    imdb_url <- "https://www.imdb.com/title/"
    imdb_url <- paste(imdb_url, movie_id, sep="")
    print(imdb_url)
    page <- read_html(imdb_url)
    span_list_item <- page %>% html_elements("span[class='ipc-metadata-list-item__list-content-item']") 
    
    box_office_item <- page %>% html_elements("li[class='ipc-metadata-list__item BoxOffice__MetaDataListItemBoxOffice-sc-40s2pl-2 gwNUHl']") 
    if(length(box_office_item) > 0) {
      revenue <- box_office_item %>% html_elements('span') %>% html_text()
      if("Opening weekend US & Canada" %in% revenue) {
        index = which(revenue == "Opening weekend US & Canada")
        revenue <- as.numeric(gsub("[^0-9]", "", revenue[index + 1]))
        revenue_date <- revenue[index + 2]
        revenue_date <- parse_date_time(revenue_date, orders = c("ybd", "dby", "bdy"))
      } else {
        revenue <-  0
        revenue_date <- NA
      }
    } else {
      revenue <-  0
      revenue_date <- NA
    }
    
    runtime_node <- page %>% html_elements("div[class='styles__MetaDataContainer-sc-12uhu9s-0 cgqHBf']")%>%html_elements('div[class = ipc-metadata-list-item__content-container]')
    runtime <- runtime_node %>% html_text()
    runtime_str <- grep('minutes', runtime, value=TRUE)
    if(length(runtime_str) == 0){
      runtime_str <- grep('minute', runtime, value=TRUE)
    }
    if(length(runtime_str) != 0){
      runtime_str <- gsub("hour", ":", runtime_str)
      runtime_str <- gsub("[^0-9 :]", "", runtime_str)
      runtime_str <- str_split(runtime_str,':')
      runtime_hour <- as.numeric(runtime_str[[1]][1])
      runtime_minutes <- as.numeric(runtime_str[[1]][2])
      total_runtime_in_minute <- runtime_hour * 60 + runtime_minutes
    } else {
      total_runtime_in_minute <- NA
    }
    
    meta_score_element <- page %>% html_elements("span[class='score-meta']") %>% html_text()
    if(length(meta_score_element) > 0){
      meta_score <- as.numeric(meta_score_element)
    } else {
      meta_score <- NA
    }
    
    meta_crtics_count <-page %>% html_elements("span[class='score']")
    if(length(meta_crtics_count) == 1){
      meta_crtics_count <- as.numeric(meta_crtics_count[1] %>%html_text())
    } else {
      meta_crtics_count <- as.numeric(meta_crtics_count[2] %>%html_text())
    }
    
    if(length(meta_crtics_count) == 0){
      meta_crtics_count <- 0
    }
  
    page_release_url <- paste(imdb_url, '/releaseinfo', sep = '')
    page_release <- read_html(page_release_url)
    release_date_node <- page_release%>%html_elements("table[class='ipl-zebra-list ipl-zebra-list--fixed-first release-dates-table-test-only']")
    release_date_table <- release_date_node%>%html_table()
    release_date_table <- data.frame(release_date_table)
    if(ncol(release_date_table) != 0) {
      release_date <- release_date_table[(release_date_table['X1'] == 'USA'), c('X2', 'X3')]
      if(length(release_date) > 1) {
        release_date <- release_date%>%filter(X3 == "" | X3 == "(internet)")%>%pull(X2)
      }
      if(length(release_date) == 1){
        release_date <- parse_date_time(release_date, orders = c("ybd", "dby", "bdy"))
      }else{
        release_date <- NA
      }
    } else {
      release_date <- NA
    }
  
    movie_imdb_df <- data.frame(movie_id, revenue, total_runtime_in_minute, meta_crtics_count, meta_score, release_date)
    movie_last5_movie_df <- rbind(movie_last5_movie_df, movie_imdb_df)
  }
  
}

# Write the data to the file
write.csv(movie_last5_movie_df,"./Group_Project/Data/movie_last5_movie_db.csv", row.names = FALSE)


