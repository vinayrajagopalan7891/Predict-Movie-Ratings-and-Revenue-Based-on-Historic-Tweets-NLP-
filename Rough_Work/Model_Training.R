library(rvest)
library(stringr)
library(tidyverse)


imbd_url <- 'https://www.imdb.com/list/ls051421138/'
html_node <- read_html(imbd_url)
h3 <- html_node %>% html_elements("h3[class='lister-item-header']")
print(h3)
movie_names <- h3%>%html_text()
movie_names <- str_to_lower(movie_names)
movie_names <- str_trim(gsub("[^a-z ]", "", movie_names))
movie_names
stopwords <- c('')
movie_name <- 'lion king'
rotten_tomatoes_lion_king_url <- "https://www.rottentomatoes.com/m/the_lion_king_2019/reviews?type=user"
lion_king_html <- read_html(rotten_tomatoes_lion_king_url)
review_div <- lion_king_html %>% html_elements("div[class='js-reviews-container reviews-movie']")
span_rating <- review_div %>% html_elements("span[class='audience-reviews__score']")
reviews <- review_div%>%html_elements("p[class='audience-reviews__review--mobile js-review-text clamp clamp-4 js-clamp']") %>% html_text()
reviews <- gsub(movie_name, "", reviews, ignore.case = TRUE)
ratings <- c()
for(span in span_rating) {
  print(span)
  span_filled <- span %>% html_elements("span[class='star-display__filled ']")
  span_half <- span %>% html_elements("span[class='star-display__half ']")
  ratings <- append(ratings, length(span_filled) + length(span_half))
}

reviews_df <- data.frame(reviews, ratings)
