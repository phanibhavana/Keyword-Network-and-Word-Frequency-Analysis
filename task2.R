
library(dplyr)
library(stringr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(tm)
library(data.table)
library(igraph)

setwd("/Users/Tejas Karwa/Desktop/Northeastern University/Fourth Year/Foundations of Data Analytics/Assignments/Project3")
file_list<-list("2010.csv","2011.csv","2012.csv","2013.csv","2014.csv","2015.csv","2016.csv","2017.csv","2018.csv","2019.csv","2020.csv","2021.csv","2022.csv")

#create data frame with 0 rows and 5 columns
df <- data.frame(matrix(ncol = 2, nrow = 0))

#provide column names
colnames(df) <- c('year', 'tweet')
#df

#cleaning data
for (j in file_list) {
  year=as.integer(substr(j, 1, 4))  
  df_temp<-fread(j,
                 select = c("tweet"))
  
  for(i in 1:nrow(df_temp)) {       # for-loop over rows
    df_temp[i,1]<-tolower(df_temp[i,1])
    df_temp[i,1]<-gsub("[[:punct:]]", "", df_temp[i,1])
    vocab <- unlist(str_split(df_temp[i,1], " "))
    vocab_nsw <- vocab[!(vocab) %in% stop_words$word] # Only keep non-stopwords
    row_clean <- paste(vocab_nsw, collapse = " ") # Gather back into one string
    #df[i,2]<-row_clean
    
    df[nrow(df) + 1,] = c(year, row_clean)
  }
}
#df

#create data frame with 0 rows and 5 columns
df_combined <- data.frame(matrix(ncol = 2, nrow = 0))

#provide column names
colnames(df_combined) <- c('year', 'combined_tweet')
#df_combined

current_year=2010
for (j in file_list){
  df_temp_year<-df%>%
    filter(df$year==current_year)
  df_combined[nrow(df_combined) + 1,] = c(current_year, (paste(df_temp_year[,2], collapse = " . ")))
  current_year=current_year+1
}
#df_combined

#create data frame with 0 rows and 5 columns
df_word_freq <- data.frame(matrix(ncol = 3, nrow = 0))

#provide column names
colnames(df_word_freq) <- c('year', 'word','freq')
#df_word_freq

current_year=2010
for (j in 1:nrow(df_combined)){ 
  insta_text_corpus <- Corpus(VectorSource(df_combined[j,2]))
  insta_2 <- TermDocumentMatrix(insta_text_corpus)
  insta_2 <- as.matrix(insta_2)
  insta_2 <- sort(rowSums(insta_2),decreasing=TRUE)
  insta_2_temp <- data.frame(year=current_year,word = names(insta_2),freq=insta_2)
  df_word_freq<-rbind(df_word_freq,insta_2_temp)
  current_year=current_year+1
}
#df_word_freq

# Word Frequency Per Year

current_year=2010
for (j in 1:13){
  print(df_word_freq%>%
          filter(year==current_year)%>%
          group_by(year)%>%
          arrange(desc(freq),.by_group = TRUE))
  current_year=current_year+1
}

# Top 10 words for each year

current_year=2010
for (j in 1:13){
  print(df_word_freq%>%
          filter(year==current_year)%>%
          group_by(year)%>%
          slice(1:10)%>%
          arrange(desc(freq),.by_group = TRUE))
  current_year=current_year+1
}

## Histogram of word frequencies
total_words <- df_word_freq %>% 
  group_by(year) %>% 
  summarize(total = sum(freq))
total_words

book_words <- left_join(df_word_freq, total_words)

#book_words

ggplot(book_words, aes(freq/total, fill = year)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~year, scales = "free_y")+theme(axis.text.x = element_text(size = 5))

##Zipf's law

# Zipf's law
freq_by_rank <- book_words %>% 
  group_by(year) %>% 
  mutate(rank = row_number(), 
         `term frequency` = freq/total) %>%
  ungroup()

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = year)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = year)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# bigrams

df_bigrams <- df %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)
#df
#df_bigrams


# Counting bigrams
df_bigrams %>%
  count(bigram, sort = TRUE)

# bigrams with stop words
bigrams_separated <- df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)



# Visualizing bigrams

#bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



