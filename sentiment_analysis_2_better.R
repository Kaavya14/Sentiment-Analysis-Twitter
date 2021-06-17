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
library(ggthemes)


# Read the text file from local machine , choose file interactively
text<- read.csv("C:/Users/KAAVYA DUGGAL/Downloads/tweets_election.csv")

# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))

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


trump<- filter(text,Subject=="Donald Trump")
head(trump)
nrow(trump)


biden<- filter(text,Subject=="Joe Biden")
head(biden)
nrow(biden)


#df <- merge(trump$state.value_counts(), biden$state.value_counts(), right_index = True, left_index = True)
df<-merge(trump, biden, by.x="state", by.y="user")
head(df,2)

df <-rename(columns = {"state_x": "Total Trump Mentions", "state_y": "Total Biden Mentions"})
df1 %>% 
  rename(
    state_x = Sepal.Length,
    sepal_width = Sepal.Width
  )

str(df)

#xxx
states_trump<-select(trump,state,text)
head(states_trump)
nrow(states_trump)
n_distinct(states_trump$state)
data_count_1 <- states_trump %>%                              
  group_by(state) %>%
  summarise(count = n_distinct(text))
head(data_count_1)
nrow(data_count_1)

data_count_1$state[which.max(data_count_1$count)]
max(data_count_1$count)
data_count_1$state[which.min(data_count_1$count)]
min(data_count_1$count)

ggplot(data=data_count_1, aes(x=state, y=count)) +
  geom_bar(stat="identity")+coord_flip()


#BIDEN
states_biden<-select(biden,state,text)
nrow(states_biden)
n_distinct(states_biden$state)
head(states_biden)

data_count_2 <- states_biden %>%                              
  group_by(state) %>%
  summarise(count = n_distinct(text))
head(data_count_2)
nrow(data_count_2)
new_row<-data.frame(state="North Dakota",count=0)
data_count_2<-rbind(data_count_2,new_row)
nrow(data_count_2)

data_count_2$state[which.max(data_count_2$count)]
max(data_count_2$count)
data_count_2$state[which.min(data_count_2$count)]
min(data_count_2$count)

ggplot(data=data_count_2, aes(x=state, y=count)) +
  geom_bar(stat="identity")+coord_flip()


#MERGING BOTH DATA FRAMES
merge_df<-data.frame(data_count_2)
head(merge_df,100)
merge_df<-rbind(merge_df,data_count_1)
new_col<-data.frame(subject="Joe Biden")
merge_df<-cbind(merge_df,new_col)
merge_df$subject[51:100]<-"Donald Trump"

#MAXIUM TWITTER MENTIONS
merge_df$state[which.max(merge_df$count)]
merge_df$subject[which.max(merge_df$count)]
max(merge_df$count)

#MINIMUM TWITTER MENTIONS
merge_df$state[which.min(merge_df$count)]
merge_df$subject[which.min(merge_df$count)]
min(merge_df$count)


ggplot(merge_df, aes(fill=subject, y=count, x=state)) + 
  geom_bar(position="dodge", stat="identity")+coord_flip()+ggtitle("Statewise Candidate Twitter Mention")+theme_classic()



