
library(shiny)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(httr)
library(textstem)
library(tidytext)
library(topicmodels)

if(!require("textstem")) install.packages("textstem"); library("textstem")
if(!require("tuber")) install.packages("tuber"); library("tuber")
if(!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")
if (!require("wordcloud")) install.packages("wordcloud"); library("wordcloud")
if(!require("httr")) install.packages("httr"); library("httr")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")

# Reading Required Data

imdb <- read.csv("./movie_db_final.csv")
movie_db <- read.csv("./movie_db.csv")
base_table <- read.csv("./base_table_pred_agg_final.csv")
wordcdf <- readRDS("./validation_final_db.rds")
summarySentiment <- read.csv("./summarySentiment.csv")
movie_data_merged__Tokenized <- readRDS("./movie_data_merged__Tokenized.rds")
movie_data_merged <- readRDS('./movie_data_merged.rds')

#View(engagement)
class(movie_data_merged__Tokenized)

engagement <- cbind(wordcdf$IMDB_Movie_ID, wordcdf$quote_count, wordcdf$reply_count, wordcdf$retweet_count, wordcdf$favorite_count)
colnames(engagement) <- c("IMDB_Movie_ID", "quote_count","reply_count","retweet_count","favorite_count")
class(engagement)
engagement <- as.data.frame(engagement)

engagement$quote_count <- as.numeric(engagement$quote_count)
engagement$reply_count <- as.numeric(engagement$reply_count)
engagement$retweet_count <- as.numeric(engagement$retweet_count)
engagement$favorite_count <- as.numeric(engagement$favorite_count)

#View(engagement_grouped)

# class(engagement)
 engagement_grouped <- engagement %>% group_by(IMDB_Movie_ID) %>%
     summarise(quote_count = sum(quote_count),
               reply_count = sum(reply_count),
             retweet_count = sum(retweet_count),
             favorite_count = sum(favorite_count))



# ---------------- TWITTER VISUALIZATION  -----------------
#Top 10 by Twitter quotes count 
top10quote <- top_n(engagement_grouped,10,quote_count)
top10quotecount <-  ggplot(top10quote, aes(x=reorder(IMDB_Movie_ID,-quote_count ), y= quote_count  ,fill = IMDB_Movie_ID)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Quote count ",x = "Movie ID", y= "Count")

top10reply <- top_n(engagement_grouped,10,reply_count)
top10replycount <-  ggplot(top10reply, aes(x=reorder(IMDB_Movie_ID,-reply_count ), y= reply_count  ,fill = IMDB_Movie_ID)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Reply count ",x = "Movie ID", y= "Count")

top10retweet <- top_n(engagement_grouped,10,retweet_count)
top10retweetcount <-  ggplot(top10retweet, aes(x=reorder(IMDB_Movie_ID,-retweet_count ), y= retweet_count  ,fill = IMDB_Movie_ID)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Retweet count ",x = "Movie ID", y= "Count")

top10fav <- top_n(engagement_grouped,10,favorite_count)
top10favcount <-  ggplot(top10fav, aes(x=reorder(IMDB_Movie_ID,-favorite_count ), y= favorite_count  ,fill = IMDB_Movie_ID)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Favourite count ",x = "Movie ID", y= "Count")



#WordCloud 

#movie_id <- movie_db[match('The White Tiger',movie_db$movie_names),"movie_imdb_id"]

#wordclouddf <- distinct(wordcdf, id, .keep_all = TRUE)

# ------WordCloud ------
wordCloudFunc<- function(movie_names){
    movie_id <- movie_db[match(movie_names, movie_db$movie_names),"movie_imdb_id"]

    tf_test <- movie_data_merged__Tokenized %>% filter(IMDB_Movie_ID == movie_id)  %>%
        group_by(word) %>%
        summarize(freq = n()) %>%
        arrange(-freq)

    if( length(tf_test$word) == 0){

        print("No tweets for this Movie")
    }
    else{

        wordcloud(tf_test$word,tf_test$freq,
                  max.words=75,
                  colors = brewer.pal(8, 'Dark2'),
                  random.order = F,
                  scale=c(3,1))
    }
}
wordCloudFunc("Swan Song")

#Sentiment Score

Sentiscore <- function(movie_names){
    movie_id <- movie_db[match(movie_names, movie_db$movie_names),"movie_imdb_id"]
    tweets_plot <- iconv((movie_data_merged %>% filter(IMDB_Movie_ID == movie_id))$text)

    s <- get_nrc_sentiment(tweets_plot)

    barplot(colSums(s),
            las = 2,
            col = rainbow(10),
            ylab = 'Count',
            main = 'Sentiment Scores of Tweets for this movie')
}

#Sentiscore('The Seventh Day')



#Summary Sentiment

summarySenti <- summarySentiment %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()

#TOPIC MODELLING
 movie_data_merged__Tokenized_1 <- movie_data_merged__Tokenized %>%
     
     anti_join(stop_words) %>%       
     
     count(IMDB_Movie_ID,word , sort=TRUE) %>%
     
     cast_dtm(document = IMDB_Movie_ID, term = word,
              
              value = n, weighting = tm::weightTf)



movies_lda <- LDA( movie_data_merged__Tokenized_1, k = 3,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
#class(movies_lda)
movie_topics <- tidy(movies_lda, matrix = "beta")

#top Terms
top_movie_terms <- movie_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
top_movie_terms
#write.csv(top_movie_terms, "./New folder/ProjectSMA/top_movie_terms.csv")

top_terms <- top_movie_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered()
class(top_terms)

# --------------------  IMDB VISUALIZATION  -------------------- #

#top 10 movies by Revenue 
t10byrevenue <- top_n(imdb, 10, revenue)
t10moviesbyrevenue <- ggplot(t10byrevenue, aes(x=reorder(movie_imdb_id,-revenue), y= revenue ,fill = movie_imdb_id)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Revenue ",x = "Movie ID", y= "Revenue")

#top 10 Movies by Metascore
t10bymetascore <- top_n(imdb, 10, meta_score)
t10moviesbymetascore <- ggplot(t10bymetascore, aes(x=reorder(movie_imdb_id,-meta_score), y= meta_score ,fill = movie_imdb_id)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Revenue ",x = "Movie ID", y= "Revenue")


#Total Runtime VS Revenue
runtimeVSrevenue <- ggplot(data=imdb, aes(x=total_runtime_in_minute, revenue,col="#69b3a2")) +
    geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title =  element_blank(),
                         panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
                         axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    geom_rug(col="steelblue",alpha=0.1, size=1.5) +
    
    labs(title = "Total Runtime vs Revenue  ",x = "Total Runtime(Minutes)", y= "Revenue")

#Metascore vs Revenue 
metascoreVSrevenue <- ggplot(data=imdb, aes(x=meta_score, revenue,col="#69b3a2")) +
    geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title =  element_blank(),
                         panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
                         axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    geom_rug(col="steelblue",alpha=0.1, size=1.5) +
    
    labs(title = "Metascore vs Revenue  ",x = "Metascore", y= "Revenue")

# ------------------------------------------------------------------------------- #

# ------------------   YOUTUBE VISUALIZATION ------------------ #

#top 10 movies with highest view count

t10Viewcount <- top_n(base_table, 10, viewCount)
t10moviesviewCount <- ggplot(t10Viewcount, aes(x=reorder(movie_imdb_id,-viewCount), y= viewCount ,fill = movie_imdb_id)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Viewcount ",x = "Movie ID", y= "Views")

t10likecount <- top_n(base_table, 10, likeCount)
t10moviesLikeCount <- ggplot(t10likecount, aes(x=reorder(movie_imdb_id,-likeCount), y= likeCount ,fill = movie_imdb_id)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Like count ",x = "Movie ID", y= "Likes")    

t10commentcount <- top_n(base_table, 10, commentCount)
t10moviescommentCount <- ggplot(t10commentcount, aes(x=reorder(movie_imdb_id,-commentCount), y= commentCount ,fill = movie_imdb_id)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Movies by Like count ",x = "Movie ID", y= "Likes")    




# TOP 10 production Houses 

production <- read.csv("./channel_final_db.csv")


#TOP 10 by View count
t10productionhouses <- top_n(production, 10, viewCount)
t10productionhouseViews <- ggplot(t10productionhouses, aes(x=reorder(prod_name.prod_name.i.,-viewCount), y= viewCount ,fill = prod_name.prod_name.i.)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Production Houses by Viewcount ",x = "Production House", y= "Views")


#Top10 by subscribers count
t10productionsubscribers <- top_n(production, 10, subscriberCount)
t10prodsubscribers <- ggplot(t10productionhouses, aes(x=reorder(prod_name.prod_name.i.,-subscriberCount), y= subscriberCount ,fill = prod_name.prod_name.i.)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Production Houses by subscribers",x = "Production House", y= "Number of Subscribers")

t10productionvideocount <- top_n(production, 10, videoCount)
t10prodvideoCount <- ggplot(t10productionvideocount, aes(x=reorder(prod_name.prod_name.i.,-videoCount), y= videoCount ,fill = prod_name.prod_name.i.)) + 
    geom_col( ) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.y=element_blank())  + theme(axis.text.x = element_text(angle = 90))+
    labs(title = "TOP 10 Production Houses by No.of Videos",x = "Production House", y= "Number of Videos")


# ------------------   YOUTUBE VISUALIZATION ------------------ #



# Define UI 

ui <-navbarPage("Movie Analytics", 
                theme = shinythemes::shinytheme("slate"),
                navbarMenu('Twitter',
                           tabPanel("SentimentScore",
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput(inputId = "sentiscore", label = "Select a Movie",
                                                        selected = 'The Seventh Day',
                                                        choices = movie_db$movie_names
                                            )),
                                        mainPanel(h3("Sentiment Score for Selected Movie",align = "center"),
                                                  plotOutput("sentiscore")))),
                           tabPanel("TOP 10 Movies by Stats",
                                    sidebarLayout(
                                        sidebarPanel(
                                            radioButtons(inputId = "radio3", label = "Twitter TOP 10 Movies ", 
                                                         choices = c("quote_count" = "t1",
                                                                     "reply_count" = "t2", "retweet_count" = "t3",
                                                                     "favourite_count" = "t4"),
                                            )),
                                        mainPanel(h3("Twitter TOP 10 Movies By Statistics",align = "center"),
                                                  plotOutput("twitstats")))),
                           tabPanel("Summary Sentiment",
                                    
                                     mainPanel(h3("Summary Sentiment",align = "center"),
                                                  plotOutput("sentiSummary"))),
                           tabPanel("Topic Modelling -Top Words",
                                    
                                    mainPanel(h3("top_terms",align = "center"),
                                              plotOutput("top_terms")))),
                           
                
                navbarMenu("Youtube",
                           tabPanel("Video Statistics",
                                    sidebarLayout(
                                        sidebarPanel(
                                            radioButtons(inputId = "radio1", label = "Movie Stats by Videos:", 
                                                         choices = c("View Count" = "v1",
                                                                     "Likes" = "v2", "Comments" = "v3")
                                            )),
                                        mainPanel(   h3("Youtube Video Statistics",align = "center"),
                                                     plotOutput('plot1')  ))),
                           
                           tabPanel("Production Channel Statistics",
                                    sidebarLayout(
                                        sidebarPanel(
                                            radioButtons(inputId = "radio2", label = "Movie Stats by Channel:", 
                                                         choices = c("Channel Views" = "c1",
                                                                     "Channel Comments" = "c2", 
                                                                     "Channel Video Count" = "c3")
                                                         
                                            )),
                                        
                                        mainPanel (
                                            plotOutput('plot2')))),
                ),
                
                tabPanel("IMDB",
                         sidebarLayout(
                             sidebarPanel(
                                 radioButtons(inputId = "imdb", label = "IMDB Movie Stats:",
                                              selected = "i1", 
                                              choices = c(
                                                  
                                                  "Top 10 Movies By Revenue" = "i1", 
                                                  "Top 10 Movies By Metascore" = "i2", 
                                                  "Runtime VS Revenue" = "i3",
                                                  "Metascore VS Revenue" = "i4")
                                 )),
                             mainPanel(h3("IMDB Movie Analysis",align = "center"),
                                       plotOutput("imdb")))),
                
                
                
                tabPanel("WordCloud",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput(inputId = "Moviename", label = "Select Movie Name",
                                             selected =  'The Little Things',
                                             choices =  movie_db$movie_names
                                             
                                 )),     
                             mainPanel(h3("Word Cloud for Selected Movie",align = "center"),
                                       plotOutput('wordcloud')))
                ))


server <- function(input, output){
    
    
    output$plot1 <- renderPlot({
        if (input$radio1 == "v1")  {print(t10moviesviewCount)}
        if (input$radio1 == "v2")  {print(t10moviesLikeCount)}
        if (input$radio1 == "v3")  {print(t10moviescommentCount)}
        
    })
    
    output$plot2 <- renderPlot({
        if (input$radio2 == "c1")  {print(t10productionhouseViews)}
        if (input$radio2 == "c2")  {print(t10prodsubscribers)}
        if (input$radio2 == "c3")  {print(t10prodvideoCount)}
        
    })
    
    
    output$imdb <- renderPlot({
        if (input$imdb == "i1")  {print(t10moviesbyrevenue)}   
        if (input$imdb == "i2")  {print(t10moviesbymetascore)}
        if (input$imdb == "i3")  {print(runtimeVSrevenue)}  
        if (input$imdb == "i4")  {print(metascoreVSrevenue)}
    })
    
    output$twitstats <- renderPlot({
        if (input$radio3 == "t1")  {print(top10quotecount)}   
        if (input$radio3 == "t2")  {print(top10replycount)}
        if (input$radio3 == "t3")  {print(top10retweetcount)}  
        if (input$radio3 == "t4")  {print(top10favcount)}
    })
    
    top10retweetcount
    
    output$wordcloud <- renderPlot({
        wordCloudFunc(input$Moviename)
    })
    
    output$sentiscore <- renderPlot({
        Sentiscore(input$sentiscore)
    })
    output$sentiSummary <- renderPlot({
        print(summarySenti)
    })
    output$top_terms <- renderPlot({
        print(top_terms)
    })
    
}



shinyApp(ui=ui, server = server)





