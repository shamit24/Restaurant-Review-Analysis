library(shiny)

library(shinyjs)

library(forcats)
library(tm)
library(stopwords)
library(caTools)
library(philentropy)
library(ngram)
library(topicmodels)
library(SnowballC)
library(wordcloud)

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
getwd()
# Read file:
chipotleON <- read.csv("chipotleinON.csv", stringsAsFactors = FALSE)
str(chipotleON)


# Build Corpus:
reviews <- (chipotleON$text)
reviews<- as.data.frame(reviews)
reviewsnum <- nrow(reviews)


## corpus prep function
prepareCorpus <- function(textArr) {
  myCorpus <- Corpus(VectorSource(textArr))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, tolower)
  myStopwords <- c(stopwords(language="en", source="smart"), 
                   "xx", "xxxx", "xxxxxxxxxxxx", "xxxxxxxx")
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, stemDocument)
  
  return (myCorpus)
}

shinyServer(function(input, output,session) {
  
 
  observeEvent(
    input$random, { 
    id <- sample.int(568, size = 1, reviewsnum)
    text <- reviews[id, "reviews"]
    updateTextInput(session, "reviewbox", value = text)
    updateTextInput(session, "response", value = "")
    
    }
    
  )
  
    
 
  
  
  observeEvent(input$analzye, {
    
    text <- input$reviewbox
    
    rcorpus <- prepareCorpus(text)
    cleanreviews<- data.frame(text = sapply(rcorpus, as.character), stringsAsFactors = FALSE)
    cleantext<- iconv(cleanreviews)
    pos_response<- "Thank you for the positive review! We are glad you enjoyed your experience at Chipotle"
    neg_response <-   "We appologize for your negative experience at Chipotle. We will take your feedback into consideration."
    
    
    
    pos <- get_nrc_sentiment(cleantext)["positive"]
    neg <- get_nrc_sentiment(cleantext)["negative"]
    if (pos > neg)
      
      updateTextInput(session, "response", value = pos_response)
      
    
    else 
      
      updateTextInput(session, "response", value = neg_response)
    
    
    
    
    
  }
               
               
               
               
               
               
               
               )
  
}
)


