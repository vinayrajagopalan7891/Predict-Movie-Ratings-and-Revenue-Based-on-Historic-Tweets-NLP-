
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

#Reading the reviews text and ratings collected from rotten tomatoes 
review_df_text <- read.csv("./Group_Project/Data/review_df.csv")
review_df_text <- review_df_text[,c('X.1','reviews','ratings')]

# Remove punctuation and numbers with regular expressions
review_df_text <- mutate(review_df_text, reviews = gsub(x = reviews, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

# Tokenization (+ going to lowercase)
review_df_Tokenized <- review_df_text %>% unnest_tokens(output = "word", # how should the new column be named?
                                                  input = reviews, # where can we find the text? 
                                                  token = "words", # which tokenization scheme should we follow?
                                                  drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase

# Remove some other elements such as # and @ signs if they might occur
review_df_Tokenized <- filter(review_df_Tokenized, substr(word, 1, 1) != '#', 
                         substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags

# remove stopwords
review_df_Tokenized <- review_df_Tokenized %>% anti_join(get_stopwords()) # note that I continue with the 'uncorrected' words here


#Lemmatization
review_df_Tokenized$word <- lemmatize_words(review_df_Tokenized$word, dictionary = lexicon::hash_lemmas)
nrow(review_df_Tokenized)

#Tweet Sentiment for Bing dictionary
reviews_sentiment_1 <- merge(review_df_Tokenized,get_sentiments(lexicon = "bing"), by.x = 'word', by.y = 'word', all.x = TRUE )

#Tweet Sentiment for Afinn dictionary
reviews_sentiment_2 <- merge(review_df_Tokenized,get_sentiments(lexicon = "afinn"), by.x = 'word', by.y = 'word', all.x = TRUE )

#Tweet Sentiment for loughran dictionary
reviews_sentiment_3 <- merge(review_df_Tokenized,get_sentiments(lexicon = "loughran"), by.x = 'word', by.y = 'word', all.x = TRUE )

#Tweet Sentiment for nrc dictionary
reviews_sentiment_4 <- merge(review_df_Tokenized, get_sentiments(lexicon = "nrc") , by.x = 'word', by.y = 'word', all.x = TRUE )


#Aggregating the results for each sentiment dictionary
reviews_sentiment_1 <- reviews_sentiment_1 %>%
  count(X.1, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment_Bing = positive - negative)


reviews_sentiment_2[is.na(reviews_sentiment_2)] <- 0
reviews_sentiment_2 <- reviews_sentiment_2 %>%
  group_by(X.1)%>%
  summarise(Sentiment_affin = sum(value))


reviews_sentiment_3 <- reviews_sentiment_3 %>%
  count(X.1, sentiment) %>%                
  spread(sentiment, n, fill = 0)

reviews_sentiment_4 <- reviews_sentiment_4 %>%
  count(X.1, sentiment) %>%                
  spread(sentiment, n, fill = 0)

#Use Udpipe to get the POS for tweets
udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)
review_df_Tokenized_udpipe <- review_df_Tokenized %>% 
  mutate(POS = purrr::map_chr(word, function(x) as.data.frame(udpipe_annotate(udmodel, x = x, tokenizer = "vertical"))$upos))  %>% 
  count(X.1, POS) %>%              
  spread(POS, n, fill = 0)
nrow(review_df_text)

#Merging all the aggregated results
base_table_df <-  merge(x = review_df_text, y = review_df_Tokenized_udpipe, by.x = "X.1", by.y = "X.1", all.x = TRUE)

base_table_df <-  merge(x = base_table_df, y = reviews_sentiment_1, by.x = "X.1", by.y = "X.1", all.x = TRUE)

base_table_df <-  merge(x = base_table_df, y = reviews_sentiment_2, by.x = "X.1", by.y = "X.1", all.x = TRUE)

base_table_df <-  merge(x = base_table_df, y = reviews_sentiment_3, by.x = "X.1", by.y = "X.1", all.x = TRUE)

base_table_df <-  merge(x = base_table_df, y = reviews_sentiment_4, by.x = "X.1", by.y = "X.1", all.x = TRUE)



set.seed(5) # Set a seed to have the same subsets every time 

# Set sample (stratified)
# Make our dependent variable dichotomous
dataset <- review_df_text
dataset[,"ratings"] <- as.factor(dataset[,"ratings"] )
y <- as.factor(dataset[,"ratings"] )
levels(y)

# Define proportion to be in training set 
p <- 0.7

# Define observations to be in training set (we use proportional sampling)
class1_train <- sample(which(y==as.integer(levels(y)[1])), floor(p*table(y)[1]),replace=FALSE)
class2_train <- sample(which(y==as.integer(levels(y)[2])), floor(p*table(y)[2]),replace=FALSE)
class3_train <- sample(which(y==as.integer(levels(y)[3])), floor(p*table(y)[3]),replace=FALSE)
class4_train <- sample(which(y==as.integer(levels(y)[4])), floor(p*table(y)[4]),replace=FALSE)
class5_train <- sample(which(y==as.integer(levels(y)[5])), floor(p*table(y)[5]),replace=FALSE)
training_locations <- c(class1_train,class2_train, class3_train, class4_train, class5_train) 


# Create a term frequency table for the training set
train_dtm <- dataset[training_locations,] %>% unnest_tokens(output = "word",
                                                                  input = reviews,
                                                                  token = "words",
                                                                  drop=FALSE,to_lower=TRUE) %>% 
  anti_join(get_stopwords()) %>%
  count(X.1,word , sort=TRUE)%>%
  cast_dtm(document = X.1, term = word,
           value = n, weighting = tm::weightTf)


# Make a vocabulary (list) of all the terms in the training table
train_vocab <- tidy(train_dtm) %>%
  distinct(term) 


test_set <- dataset[-training_locations,]
# Create a term frequency table for the test set
test_table <- test_set %>% unnest_tokens(output = "word",
                                         input = reviews,
                                         token = "words",
                                         drop=FALSE,to_lower=TRUE) %>%  
  anti_join(get_stopwords()) %>%
  count(X.1,word , sort=TRUE)

test_table <- test_table %>%
  right_join(train_vocab,by=c("word"="term"))


test_dtm <- test_table %>% 
  arrange(desc(X.1)) %>% 
  mutate(id = ifelse(is.na(X.1), first(X.1), X.1),
         n = ifelse(is.na(n), 0, n)) %>% 
  cast_dtm(document=X.1, term=word, value=n)


in_dtm <- rownames(test_dtm)

lostids <- test_set[! test_set$X.1 %in% in_dtm,"X.1"]


test_dtm <- rbind(test_dtm,matrix(data=0,nrow=length(lostids),ncol=ncol(test_dtm)))

# add the rownames of the originally missing observations as well
rownames(test_dtm)[(nrow(test_dtm)-length(lostids)+1):nrow(test_dtm)] <- paste(lostids)


dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}

train=dtm.to.sm(train_dtm)

# define k, which is the number of concepts to keep  
k = 20
trainSVD <- irlba(t(train), nu=k, nv=k)
train <- as.data.frame(trainSVD$v)

train <- data.frame(cbind(id = rownames(train_dtm),train),stringsAsFactors=FALSE)

test=dtm.to.sm(test_dtm)
test <- as.data.frame(as.matrix(test %*% trainSVD$u %*%  solve(diag(trainSVD$d))))

# add our rownames again, as columns, in order to be able to merge
test <- cbind(id = rownames(test_dtm),test,stringsAsFactors=FALSE)

base_table_df$reviews <- NULL
base_table_df$X <- NULL

train_final <- merge(base_table_df[training_locations,], train , by.y = "id", by.x= "X.1", x.all = TRUE)

test_final <- merge(base_table_df[-training_locations,], test,  by.y = "id", by.x= "X.1", x.all = TRUE)
train_final$`<NA>`<- NULL
train_final$`<NA>.x`<- NULL
train_final$`<NA>.y`<- NULL
train_final[is.na(train_final)] <- 0

gc()

#Traing the model
#Random Forrest
traing_ratings <- train_final[, c('ratings', 'X.1')]
RF_model_train <- randomForest(x=train_final[,3:ncol(train_final)],y=traing_ratings$ratings,importance=TRUE,ntree=1000)
save(RF_model_train, file="RF_model_train.Rdata")

test_final$`<NA>`<- NULL
test_final$`<NA>.x`<- NULL
test_final$`<NA>.y`<- NULL
test_final[is.na(test_final)] <- 0
str(test_final[,3:ncol(test_final)])

library(pROC)
RF_predict <- predict(RF_model_train,test_final[,3:ncol(test_final)])
RF_predict_f <- factor(ceiling(RF_predict), levels=c(1,2,3, 4, 5))
test_final_r_f <- factor(test_final$ratings, levels=c(1,2,3, 4, 5))
r <- multiclass.roc(test_final$ratings, RF_predict, levels=c(1,2,3, 4, 5))
confusionMatrix(test_final_r_f, RF_predict_f )
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

#Neural Network
n <- names(train_final)
str(train_final)
f <- as.formula(paste("ratings ~", paste(n[!n %in% c("X.1","ratings")], collapse = " + ")))
f
nn <- neuralnet(f, data = train_final, hidden = c(1000, 100, 100), act.fct = "logistic", linear.output = FALSE,lifesign = "minimal")
str(test_final[,3:ncol(test_final)])
nn_predict <- predict(nn,test_final[,3:ncol(test_final)])
nn_predict_f <- factor(ceiling(nn_predict), levels=c(1,2,3, 4, 5))
test_final_r_f <- factor(test_final$ratings, levels=c(1,2,3, 4, 5))
confusionMatrix(test_final_r_f, nn_predict_f )


##linear Regression
lm_model_1 <- lm(f, data = train_final)
str(test_final[,3:ncol(test_final)])
lm_predict <- predict(lm_model_1,test_final[,3:ncol(test_final)])
lm_predict_f <- factor(ceiling(lm_predict), levels=c(1,2,3, 4, 5))
test_final_r_f <- factor(test_final$ratings, levels=c(1,2,3, 4, 5))
r <- multiclass.roc(test_final$ratings, lm_predict, levels=c(1,2,3, 4, 5))
confusionMatrix(test_final_r_f, lm_predict_f )
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

summary(lm_model)

##decison tree
library(rpart)
library(tree)
tree <- rpart(f, data = train_final)
tree_predict <- predict(tree, test_final[,3:ncol(test_final)])
tree_predict_f <- factor(ceiling(tree_predict), levels=c(1,2,3, 4, 5))
test_final_r_f <- factor(test_final$ratings, levels=c(1,2,3, 4, 5))
r <- multiclass.roc(test_final$ratings,tree_predict, levels=c(1,2,3, 4, 5))
confusionMatrix(test_final_r_f, tree_predict_f)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

