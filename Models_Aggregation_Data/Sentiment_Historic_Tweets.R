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

#Read the historic tweets for which we need to predict labels
tweets_historic <- readRDS("./Group_Project/Data/historical_data_df.rds")
tweets_historic_text <- mutate(tweets_historic, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


# Tokenization (+ going to lowercase)
tweets_historic_Tokenized <- tweets_historic_text %>% unnest_tokens(output = "word", # how should the new column be named?
                                                                    input = text, # where can we find the text? 
                                                                    token = "words", # which tokenization scheme should we follow?
                                                                    drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase
# Remove some other elements such as # and @ signs if they might occur
tweets_historic_Tokenized <- filter(tweets_historic_Tokenized, substr(word, 1, 1) != '#', 
                                    substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags

# remove stopwords
tweets_historic_Tokenized <- tweets_historic_Tokenized %>% anti_join(get_stopwords()) # note that I continue with the 'uncorrected' words here


#Lemmatization
tweets_historic_Tokenized$word <- lemmatize_words(tweets_historic_Tokenized$word, dictionary = lexicon::hash_lemmas)


#Tweet Sentiment for Bing dictionary
tweets_historic_sentiment_1 <- merge(tweets_historic_Tokenized,get_sentiments(lexicon = "bing"), by.x = 'word', by.y = 'word', all.x = TRUE )

#Tweet Sentiment for Afinn dictionary
tweets_historic_sentiment_2 <- merge(tweets_historic_Tokenized,get_sentiments(lexicon = "afinn"), by.x = 'word', by.y = 'word', all.x = TRUE )

#Tweet Sentiment for loughran dictionary
tweets_historic_sentiment_3 <- merge(tweets_historic_Tokenized,get_sentiments(lexicon = "loughran"), by.x = 'word', by.y = 'word', all.x = TRUE )

#Tweet Sentiment for nrc dictionary
tweets_historic_sentiment_4 <- merge(tweets_historic_Tokenized, get_sentiments(lexicon = "nrc") , by.x = 'word', by.y = 'word', all.x = TRUE )



#Aggregating the results for each sentiment dictionary
tweets_historic_sentiment_1 <- tweets_historic_sentiment_1 %>%
  count(id, sentiment) %>%                
  spread(sentiment, n, fill = 0) %>%       
  mutate(sentiment_Bing = positive - negative)



tweets_historic_sentiment_2[is.na(tweets_historic_sentiment_2)] <- 0
tweets_historic_sentiment_2 <- tweets_historic_sentiment_2 %>%
  group_by(id)%>%
  summarise(Sentiment_affin = sum(value))



tweets_historic_sentiment_3 <- tweets_historic_sentiment_3 %>%
  count(id, sentiment) %>%                
  spread(sentiment, n, fill = 0)


tweets_historic_sentiment_4 <- tweets_historic_sentiment_4 %>%
  count(id, sentiment) %>%                
  spread(sentiment, n, fill = 0)


#Use Udpipe to get the POS for tweets
udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)
tweets_historic_Tokenized_udpipe <- tweets_historic_Tokenized %>% 
  mutate(POS = purrr::map_chr(word, function(x) as.data.frame(udpipe_annotate(udmodel, x = x, tokenizer = "vertical"))$upos))  %>% 
  count(id, POS) %>%              
  spread(POS, n, fill = 0)


#Merging all the aggregated results
base_table_df <-  merge(x = tweets_historic_text, y = tweets_historic_Tokenized_udpipe, by.x = "id", by.y = "id", all.x = TRUE)

base_table_df <-  merge(x =base_table_df, y = tweets_historic_sentiment_1, by.x = "id", by.y = "id", all.x = TRUE)

base_table_df <-  merge(x = base_table_df, y = tweets_historic_sentiment_2, by.x = "id", by.y = "id", all.x = TRUE)

base_table_df <-  merge(x = base_table_df, y = tweets_historic_sentiment_3, by.x = "id", by.y = "id", all.x = TRUE)

base_table_df <-  merge(x = base_table_df, y = tweets_historic_sentiment_4, by.x = "id", by.y = "id", all.x = TRUE)


saveRDS(base_table_df,file = "./Group_Project/Data/base_table_tweet_sentiment_db.rds")

#Initialing dataset for feature reduction
dataset <- tweets_historic_text
validation_table <- dataset %>% unnest_tokens(output = "word",
                                         input = text,
                                         token = "words",
                                         drop=FALSE,to_lower=TRUE) %>%  
  anti_join(get_stopwords()) %>%
  count(id,word , sort=TRUE)

validation_table <- validation_table %>% right_join(train_vocab,by=c("word"="term"))
validation_dtm <- validation_table %>% 
  arrange(desc(id)) %>% 
  mutate(id = ifelse(is.na(id), first(id), id),
         n = ifelse(is.na(n), 0, n)) %>% 
  cast_dtm(document=id, term=word, value=n)

in_dtm <- rownames(validation_dtm)

lostids <- test_set[! dataset$id %in% in_dtm,"id"]

validation_dtm <- rbind(validation_dtm,matrix(data=0,nrow=length(lostids),ncol=ncol(validation_dtm)))

validation=dtm.to.sm(validation_dtm)


validation <- as.data.frame(as.matrix(validation %*% trainSVD$u %*%  solve(diag(trainSVD$d))))

# add our rownames again, as columns, in order to be able to merge
validation <- cbind(id = rownames(validation_dtm),validation,stringsAsFactors=FALSE)

#Merging features for running the model

validation_final <- merge(base_table_df, validation,  by.y = "id", by.x= "id", x.all = TRUE)

validation_final$`<NA>`<- NULL
validation_final$`<NA>.x`<- NULL
validation_final$`<NA>.y`<- NULL
validation_final[is.na(validation_final)] <- 0

#Predicting Values based on models trained in SentimentReview.R
lm_predict_validation <- predict(lm_model,validation_final[,3:ncol(validation_final)])
lm_predict_f <- factor(ceiling(lm_predict_validation), levels=c(1,2,3, 4, 5))


validation_final_db <- cbind(validation_final, lm_predict_f)
saveRDS(validation_final_db,file = "./Group_Project/Data/validation_final_db.rds")
