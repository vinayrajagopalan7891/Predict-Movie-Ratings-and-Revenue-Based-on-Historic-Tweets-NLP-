# Initialize library
library(tidyverse)
library(rvest)
library(lubridate)
library("XML")
library("xml2")
library(stringr)
library(hash)

#Scrape Imdb for each movie to fetch revenues, total_runtime_in_minutes, meta_crtics_counts, meta_scores, release_dates#####################################
# Read the base the movies base table for movies released in 2021
base_movie_df <- read.csv("./Group_Project/Data/movie_db.csv", header=TRUE)
View(base_movie_df)

# Fetch movie ids for the movie
movie_ids <- base_movie_df$movie_imdb_id

# Initialize the arrays to which values will be apppended
revenues <- c()
revenue_dates <- c()
number_of_nominations <- c()
total_runtime_in_minutes <- c()
release_dates <- c()
meta_scores <- c()
meta_crtics_counts <- c()

#Creating a dataframe with movie_ids as index. 
movie_imdb <- data.frame(movie_ids)

#Iterate through the for loop and scrape IMDB page for each movie to get all the key values for the movie
for(movie_id in movie_ids){
  #Initialize the Url to scrape
  imdb_url <- "https://www.imdb.com/title/"
  imdb_url <- paste(imdb_url, movie_id, sep="")
  print(imdb_url)
  page <- read_html(imdb_url)
  
  #Fetch all the span elements.
  span_list_item <- page %>% html_elements("span[class='ipc-metadata-list-item__list-content-item']") 
  
  #Fetch all the list elements for the Box Office section of the movie page
  box_office_item <- page %>% html_elements("li[class='ipc-metadata-list__item BoxOffice__MetaDataListItemBoxOffice-sc-40s2pl-2 gwNUHl']") 
  # Check if there are elements associated with Box Office
  if(length(box_office_item) > 0) {
    revenue <- box_office_item %>% html_elements('span') %>% html_text()
    if("Opening weekend US & Canada" %in% revenue) {
      index = which(revenue == "Opening weekend US & Canada")
      revenue <- as.numeric(gsub("[^0-9]", "", revenue[index + 1]))
      revenues <- append(revenues, revenue)
      revenue_date <- revenue[index + 2]
      revenue_date <- parse_date_time(revenue_date, orders = c("ybd", "dby", "bdy"))
      revenue_dates <- append(revenue_dates, revenue_date)
    } else {
      revenues <- append(revenues, 0)
      revenue_dates <- append(revenue_dates, NA)
    }
  } else {
    revenues <- append(revenues, 0)
    revenue_dates <- append(revenue_dates, NA)
  }
  
  #Fetch the div tag for runtime
  runtime_node <- page %>% html_elements("div[class='styles__MetaDataContainer-sc-12uhu9s-0 cgqHBf']")%>%html_elements('div[class = ipc-metadata-list-item__content-container]')
  runtime <- runtime_node %>% html_text()
  runtime_str <- grep('minutes', runtime, value=TRUE)
  # Check if there are elements associated with Runtime
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
    total_runtime_in_minutes <- append(total_runtime_in_minutes, total_runtime_in_minute)
  } else {
    total_runtime_in_minutes <- append(total_runtime_in_minutes, NA)
  }
  
  #Fetch the span tag for meta score
  meta_score_element <- page %>% html_elements("span[class='score-meta']") %>% html_text()
  if(length(meta_score_element) > 0){
    meta_score <- as.numeric(meta_score_element)
  } else {
    meta_score <- NA
  }
  meta_scores <- append(meta_scores, meta_score)
  
  #Fetch the span tag for meta critcs count
  meta_crtics_count <-page %>% html_elements("span[class='score']")
  if(length(meta_crtics_count) == 1){
    meta_crtics_count <- as.numeric(meta_crtics_count[1] %>%html_text())
  } else {
    meta_crtics_count <- as.numeric(meta_crtics_count[2] %>%html_text())
  }
  meta_crtics_counts <- append(meta_crtics_counts, meta_crtics_count)
  
  
  #Fetch the release information asssociated with the movie
  page_release_url <- paste(imdb_url, '/releaseinfo', sep = '')
  page_release <- read_html(page_release_url)
  release_date_node <- page_release%>%html_elements("table[class='ipl-zebra-list ipl-zebra-list--fixed-first release-dates-table-test-only']")
  release_date_table <- release_date_node%>%html_table()
  release_date_table <- data.frame(release_date_table)
  release_date <- release_date_table[(release_date_table['X1'] == 'USA'), c('X2', 'X3')]
  #Check if a valid element exists
  if(length(release_date) > 1) {
    release_date <- release_date%>%filter(X3 == "" | X3 == "(internet)")%>%pull(X2)
  }
  if(length(release_date) == 1){
    release_date <- parse_date_time(release_date, orders = c("ybd", "dby", "bdy"))
  }else{
    release_date <- NA
  }
  release_dates <- append(release_dates, release_date)
  
  number_of_nomination <- span_list_item[1] %>% html_text()
  number_of_nomination <- as.numeric(gsub("[^0-9]", "", number_of_nominations))
  number_of_nominations <- append(number_of_nominations, number_of_nomination)
}

#Write all the columns to the data frame
movie_imdb_df <- data.frame(movie_ids, revenues, total_runtime_in_minutes, meta_crtics_counts, meta_scores, release_dates)
View(movie_imdb_df)
# Write data to the table
write.csv(movie_imdb_df,"./Group_Project/Data/movie_imdb_db.csv", row.names = FALSE)
