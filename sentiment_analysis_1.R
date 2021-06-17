# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
install.packages("tidytext")

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library(dslabs)
library(tidyverse)
library(tidytext)

# Read the text file 
text<- read.csv("C:/Users/KAAVYA DUGGAL/Downloads/tweets_election.csv")

# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))

#Observe Dataset
head(text,5)
tail(text,5)
str(text)
names(text)
nrow(text)
is.na(text)

#CLEANING THE DATA
#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))

# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)



#1) BUILDING THE TERM DOCUMENT MATRIX
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)

# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

# Display the top 20 most frequent words in entire dataset
head(dtm_d, 20)

# Plot the most frequent words
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$word,
        col ="lightgreen", main ="Top 20 most frequent words",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


# Find associations 
findAssocs(TextDoc_dtm, terms = c("campaign"), corlimit =0.9999999999999)
findAssocs(TextDoc_dtm, terms = c("white"), corlimit =0.999)	





#EVERYTHING FOR TRUMP-------------------------------------
trump<- filter(text,Subject=="Donald Trump")
head(trump)
nrow(trump)

trumpCorpus <- Corpus(VectorSource(trump))
TextDoc_trump<-TermDocumentMatrix(trumpCorpus)
dtm_trump<-as.matrix(TextDoc_trump)
dtm_dtv <- sort(rowSums(dtm_trump),decreasing=TRUE)
dtm_dtd <- data.frame(word = names(dtm_dtv),freq=dtm_dtv)

# Display the top 30 most frequent words for TRUMP
head(dtm_dtd,30)

# Plot the most frequent words
barplot(dtm_dtd[1:30,]$freq, las = 2, names.arg = dtm_dtd[1:30,]$word,
        col ="lightgreen", main ="Top 30 most frequent words for Trump",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1235)
wordcloud(words = dtm_dtd$word, freq = dtm_dtd$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_trump, terms = c("donald"), corlimit =0.9999999999999999999999999)
findAssocs(TextDoc_trump, terms = c("new"), corlimit =0.9)
findAssocs(TextDoc_trump, terms = c("hunter"), corlimit =0.99999999)




#EVERYTHING FOR BIDEN-------------------------------------
biden<- filter(text,Subject=="Joe Biden")
head(biden)
nrow(biden)

bidenCorpus <- Corpus(VectorSource(biden))
TextDoc_biden<-TermDocumentMatrix(bidenCorpus)
dtm_biden<-as.matrix(TextDoc_biden)
dtm_jbv <- sort(rowSums(dtm_biden),decreasing=TRUE)
dtm_jbd <- data.frame(word = names(dtm_jbv),freq=dtm_jbv)

# Display the top 30 most frequent words for BIDEN
head(dtm_jbd,30)

# Plot the most frequent words
barplot(dtm_jbd[1:30,]$freq, las = 2, names.arg = dtm_jbd[1:30,]$word,
        col ="lightgreen", main ="Top 30 most frequent words for Biden",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1236)
wordcloud(words = dtm_jbd$word, freq = dtm_jbd$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_biden, terms = c("joe"), corlimit =0.99999999999999999999999)
findAssocs(TextDoc_biden, terms = c("nbc"), corlimit =0.99999999999999999999999)
















