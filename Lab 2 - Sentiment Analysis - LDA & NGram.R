library(tidyverse)
library(tm)

# Read file:
chipotleON <- read.csv("chipotleinON.csv", stringsAsFactors = FALSE)
str(chipotleON)
sapply(chipotleON, class)


# Build Corpus:
reviews <- iconv(chipotleON$text, to = "utf-8")
corpus <- Corpus(VectorSource(reviews))
inspect(corpus)
inspect(corpus[1:5])



# Clean text:
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

inspect(cleanset[62])
cleanset <- tm_map(cleanset, content_transformer(gsub), pattern = "momtrãfâ©al", replacement = "montreal")
inspect(cleanset[62])

cleanset <- tm_map(cleanset, gsub, pattern = 'bowls', replacement = 'bowl')
cleanset <- tm_map(cleanset, gsub, pattern = 'burritos', replacement = 'burrito')


inspect(cleanset[1])
cleanset <- tm_map(cleanset, removeWords, c('ahhhhhhh'))
inspect(cleanset[1])

inspect(cleanset[71])
cleanset <- tm_map(cleanset, removeWords, c('ahhhhh'))
inspect(cleanset[71])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

############################ WORD CLOUD #########################################


# Term Document Matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]


# Bar Plot
w <- rowSums(tdm)
print(w)

w <- subset(w, w>=50)
print(w)

barplot(w,
        las = 2,
        col = rainbow(50))


#Word Cloud:
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)

wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.3)



###################### Topic Modeling - Latent Dirichlet Allocation (LDA) ####################

library(topicmodels)

# Create Document-Term Matrix
dtm <- DocumentTermMatrix(cleanset)
dtm


#Using LDA() function from the topicsmodels package, setting k=2, to create a two - topic LDA model: 

ap_lda <- LDA(dtm, k = 2, control = list(seed = 1234))
ap_lda

# Using tidytext package to extract the per-topic-per-word probabilities, called ?? ("beta"), from the model:

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# Notice that the above has turned the model into a one-topic-per-term-per-row format:
# Using dplyr's top_n() we can find the 10 terms that are most common within each topic:

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)

ap_top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()



# To constrain it to a set of especially relevant words, we can filter for relatively-
# -common words, such as those that have a ?? greater than 1/1000 in at least one topic.

library(tidyr)

beta_spread <- ap_topics %>%
        mutate(topic = paste0("topic", topic)) %>%
        spread(topic, beta) %>%
        filter(topic1 > .001 | topic2 > .001) %>%
        mutate(log_ratio = log2(topic2 / topic1))

beta_spread


# The words with the greatest difference b/w the two topics are visualized in the following: 

beta_spread %>%
        group_by(direction = log_ratio > 0) %>%
        top_n(10, abs(log_ratio)) %>%
        ungroup() %>%
        mutate(term = reorder(term, log_ratio)) %>%
        ggplot(aes(term, log_ratio)) +
        geom_col() +
        labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
        coord_flip()


# Document-Topic Probabilities: using LDA to model each document as a mixture of topics:
# We can examine the per-document-per-topic probabilities, called ?? ("gamma"), with the matrix = "gamma" argument to tidy()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# Use following word to check what are the most common words in any given document:

tidy(dtm) %>%
        filter(document == 10) %>%
        arrange(desc(count))
        

##############################################################################################


# Tidying DocumentTermMatrix objects - For Data Analysis:

library(tm)

# We can access the terms in the document with the Terms() function. 
dtm
terms <- Terms(dtm)
head(terms)

# Use the following to analyze the data:

library(dplyr)
library(tidytext)

ap_td <- tidy(dtm)
ap_td

# Using the following we can perform sentiment analysis on the documents: 

ap_sentiments <- ap_td %>%
        inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments


# Words from Chipotle reviews with the greatest contribution to positive or negative sentiments, using the Bing sentiment lexicon 

library(ggplot2)

ap_sentiments %>%
        count(sentiment, term, wt = count) %>%
        ungroup() %>%
        filter(n >= 20) %>%
        mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
        mutate(term = reorder(term, n)) %>%
        ggplot(aes(term, n, fill = sentiment)) +
        geom_bar(stat = "identity") +
        ylab("Contribution to sentiment") +
        coord_flip()


####################### Relationships between words: n-gram ###################################

# n-grams function is used to tokenize words into consecutive sequences of words.
# By seeing how often word X is followed by word Y, we can then build a model of the relationships between them.
# We do this by adding the token = "ngrams" option to unnest_tokens(), and setting n to the number of words we wish to capture in each n-gram.
# When we set n to 2, we are examining pairs of two consecutive words, often called "bigrams":
# In some analyses you may be interested in the most common trigrams, which are consecutive sequences of 3 words. We can find this by setting n = 3:

library(dplyr)
library(tidytext)

chipotleON_bigrams <- chipotleON %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2)

chipotleON_bigrams

# Counting and filtering n-grams
# We can examine the most common bigrams using dplyr's count():

chipotleON_bigrams %>%
        count(bigram, sort = TRUE)

# As one might expect, a lot of the most common bigrams are pairs of common (uninteresting) words, 
# such as "in the" and "of the": what we call "stop-words". This is a useful time to use tidyr's separate(), 
# which splits a column into multiple based on a delimiter. This lets us separate it into two columns,
# "word1" and "word2", at which point we can remove cases where either is a stop-word.

library(tidyr)

bigrams_separated <- chipotleON_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort = TRUE)

bigram_counts #We can see that names are the most common pairs in ChipotleON-Reviews dataset:

# tidyr's unite() function is the inverse of separate(), and lets us recombine the columns
# into one. Thus, "separate/filter/count/unite" let us find the most common bigrams 
# not containing stop-words.


bigrams_united <- bigrams_filtered %>%
        unite(bigram, word1, word2, sep = " ")

bigrams_united


# Let's find out the most common TRIGRAMS, which are consecutive sequences of 3 words.
# We can find this by setting n = 3:

chipotleON %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word,
               !word3 %in% stop_words$word) %>%
        count(word1, word2, word3, sort = TRUE)


# Using bigrams to provide context in sentiment analysis
# For example, the words "happy" and "like" will be counted as positive, 
# even in a sentence like "I'm not happy and I don't like it!"
# Now that we have the data organized into bigrams,
# it's easy to tell how often words are preceded by a word like "not":
# By performing sentiment analysis on the bigram data, we can examine how often 
# sentiment-associated words are preceded by "not" or other negating words. 
# We could use this to ignore or even reverse their contribution to the sentiment score.

bigrams_separated %>%
        filter(word1 == "not") %>%
        count(word1, word2, sort = TRUE)

# Let's use the AFINN lexicon for sentiment analysis, which gives a numeric sentiment value
# for each word, with positive or negative numbers indicating the direction of the sentiment:
# We can then examine the most frequent words that were preceded by "not" and were
# associated with a sentiment.

library(textdata)

AFINN <- get_sentiments("afinn")
AFINN

not_words <- bigrams_separated %>%
        filter(word1 == "not") %>%
        inner_join(AFINN, by = c(word2 = "word")) %>%
        count(word2, value, sort = TRUE)

not_words 

# Here the most common sentiment-associated word to follow "not" was "worth",
# which would normally have a (positive) score of 2.

#  A bar plot to visualize which words contributed the most in the "wrong" direction
library(ggplot2)

not_words %>%
        mutate(contribution = n * value) %>%
        arrange(desc(abs(contribution))) %>%
        head(20) %>%
        mutate(word2 = reorder(word2, contribution)) %>%
        ggplot(aes(word2, n * value, fill = n * value > 0)) +
        geom_col(show.legend = FALSE) +
        xlab("Words preceded by \"not\"") +
        ylab("Sentiment value * number of occurrences") +
        coord_flip()

# "Not" isn't the only term that provides some context for the following word.
# We could pick four common words (or more) that negate the subsequent term, 
# and use the same joining and counting approach to examine all of them at once.


negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
        filter(word1 %in% negation_words) %>%
        inner_join(AFINN, by = c(word2 = "word")) %>%
        count(word1, word2, value, sort = TRUE)


# Visualize what the most common words to follow each particular negation

negated_words %>%
        mutate(contribution = n * value,
               word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
        group_by(word1) %>%
        top_n(12, abs(contribution)) %>%
        ggplot(aes(word2, contribution, fill = n * value > 0)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ word1, scales = "free") +
        scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
        xlab("Words preceded by negation term") +
        ylab("Sentiment value * # of occurrences") +
        coord_flip()


# Note - Using bigram we actually double the total size of our matrix!
# Sparsity problem - Most of the cell in your matrix are going to be empty,
# because most of the times, most of the documents are not going to share the same bigram
# And as you add more powerful feature like trigram & fourgram etc., it is even less likely 
# that particular word order are going to be shared across multiple documents.  
# So Sparsity problem exist where matrix is huge because of lots of columns.



#################################### Sentiment Analysis ###################################

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


# Obtain sentiment scores:
s <- get_nrc_sentiment(reviews)
head(s)


# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Chipotle reviews')


##############################################################################################################
