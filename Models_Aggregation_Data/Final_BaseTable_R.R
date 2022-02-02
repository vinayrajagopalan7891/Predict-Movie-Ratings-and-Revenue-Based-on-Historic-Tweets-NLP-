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

#Initializing the libraries
tweets_historic <- readRDS("./Group_Project/Data/validation_final_db.rds")
movie_db_final <- read.csv("./Group_Project/Data/movie_db_final.csv")

#Merge the tables to create base table
final_db <- merge(x = movie_db_final, y = tweets_historic, by.x = "movie_imdb_id", by.y = "IMDB_Movie_ID", all.x = TRUE)
#Remove Duplicates
final_db_unique <- distinct(final_db, id, .keep_all = TRUE)

#Set the factor level for predicted sentiment/review label
final_db_unique$lm_predict_f = factor(final_db_unique$lm_predict_f, levels=c(levels(final_db_unique$lm_predict_f), 0))
final_db_unique$lm_predict_f[is.na(final_db_unique$lm_predict_f)] = 0

#Initializing not in function
`%not_in%` <- purrr::negate(`%in%`)

#Will keep track of aggregated columns so that duplicates are not added
colnames_added <- c()
base_table_pred <- data.frame()

#Aggregate results for columns 37:72 and append the results in base table
for(index in 37:72){
  if(colnames(final_db)[index] %not_in% colnames_added){
    colnames_added <- append(colnames_added, colnames(final_db)[index])
    col_name <- paste0("agg_",colnames(final_db)[index])
    agg_result <- final_db_unique %>%group_by(movie_imdb_id)%>%summarise(col_name = mean(!!as.name(colnames(final_db)[index]), na.rm = TRUE))
    colnames(agg_result)[which(names(agg_result) == "col_name")] <- col_name
    if(nrow(base_table_pred) == 0){
      base_table_pred <- merge(x = movie_db_final, y = agg_result, by.x = "movie_imdb_id", by.y = "movie_imdb_id", all.x  = TRUE)
    } else {
      base_table_pred <- merge(x = base_table_pred, y = agg_result, by.x = "movie_imdb_id", by.y = "movie_imdb_id", all.x  = TRUE)
    }
  }
}

#Aggregate results for columns 28:31 and append the results in base table
for(index in 28:31){
  if(colnames(final_db)[index] %not_in% colnames_added){
    colnames_added <- append(colnames_added, colnames(final_db)[index])
    col_name <- paste0("agg_",colnames(final_db)[index])
    agg_result <- final_db_unique %>%group_by(movie_imdb_id)%>%summarise(col_name = mean(!!as.name(colnames(final_db)[index]), na.rm = TRUE))
    colnames(agg_result)[which(names(agg_result) == "col_name")] <- col_name
    if(nrow(base_table_pred) == 0){
      base_table_pred <- merge(x = movie_db_final, y = agg_result, by.x = "movie_imdb_id", by.y = "movie_imdb_id", all.x  = TRUE)
    } else {
      base_table_pred <- merge(x = base_table_pred, y = agg_result, by.x = "movie_imdb_id", by.y = "movie_imdb_id", all.x  = TRUE)
    }
  }
}

#Aggregate results for columns 93 and append the results in base table
for(index in 93){
  if(colnames(final_db)[index] %not_in% colnames_added){
    colnames_added <- append(colnames_added, colnames(final_db)[index])
    col_name <- paste0("agg_",colnames(final_db)[index])
    agg_result <- final_db_unique %>%group_by(movie_imdb_id)%>%summarise(col_name = mean(as.numeric(!!as.name(colnames(final_db)[index])), na.rm = TRUE))
    colnames(agg_result)[which(names(agg_result) == "col_name")] <- col_name
    if(nrow(base_table_pred) == 0){
      base_table_pred <- merge(x = movie_db_final, y = agg_result, by.x = "movie_imdb_id", by.y = "movie_imdb_id", all.x  = TRUE)
    } else {
      base_table_pred <- merge(x = base_table_pred, y = agg_result, by.x = "movie_imdb_id", by.y = "movie_imdb_id", all.x  = TRUE)
    }
  }
}

#Reading Youtube video stats to merge to base table
video_df_final <- read.csv("./Group_Project/Data/video_df_final.csv")
#Renaming id column
names(video_df_final)[names(video_df_final) == 'movie_ids.movie.'] <- 'movie_imdb_id'

base_table_pred_agg <- merge(x = base_table_pred, y = video_df_final, by = "movie_imdb_id", all.x = TRUE)
#Renaming predicted label column
names(base_table_pred_agg)[names(base_table_pred_agg) == 'lm_predict_f'] <- 'predicted_ratings'
write.csv(base_table_pred_agg,"./Group_Project/Data/base_table_pred_agg_final.csv", row.names = FALSE)

base_table_pred_agg$X <- NULL
base_table_final <- base_table_pred_agg[base_table_pred_agg$revenue > 0,]
col_to_remove <- c()

#Check for numeric columns
for(col in colnames(base_table_final)){
  if(col == 'movie_imdb_id'){
    next
  }
  if(class(base_table_pred_agg[,col]) == 'numeric' | class(base_table_pred_agg[,col]) == 'integer'){
    base_table_final[,col][is.na(base_table_final[,col])] <- median(base_table_final[,col], na.rm=TRUE)
    base_table_final[,col] <- scale(base_table_final[,col])
  } else {
    col_to_remove <- append(col_to_remove, col)
  }
}

#Remove numeric columns
for(col in col_to_remove){
  base_table_final[,col] <- NULL
}
set.seed(5)


## 75% of the sample size
smp_size <- floor(0.60 * nrow(base_table_final))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(base_table_final)), size = smp_size)
train <- base_table_final[train_ind, ]
test <- base_table_final[-train_ind, ]
n <- names(base_table_final)
f <- as.formula(paste("revenue ~", paste(n[!n %in% c("movie_imdb_id","revenue")], collapse = " + ")))

##linear regression
lm_model <- lm(f, data = train)
lm_predict <- predict(lm_model,test[,3:ncol(test)])
r <- multiclass.roc(test$revenue, lm_predict)
test_roc = roc(test$revenue ~ lm_predict, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)



