
library(tokenizers.bpe)
library(dplyr)
library(stopwords)
library(tidytext)
library(stringi)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(widyr)
library(ggraph)
library(igraph)
library(quanteda)
library(topicmodels)
library(cvTools)


# laRegenta <- read.delim("laRegenta.txt", encoding = "UTF-8")

lines <- readLines("laRegenta.txt", encoding = "UTF-8")
grep(pattern = "***", lines, fixed = TRUE)
head(lines, 42) # Before prologue
tail(lines, 366) # Ending
lRegenta <- lines[42:34292]
length(lRegenta) # text without pre-prologue and post-ending
head(lRegenta)
tail(lRegenta)


# Clean data
lRegenta
lRegenta <- gsub("\\r", " ", lRegenta) # remove CR (Carriage Return)
head(lRegenta)
lRegenta <- gsub("\\s\\s", " ", lRegenta) # remove double spaces
head(lRegenta)
lRegenta <- gsub("\\n", " ", lRegenta) # remove new lines
head(lRegenta)
lRegenta <- gsub("\\s\\.", "\\.", lRegenta) # go from 'something .' to 'something.'
head(lRegenta)
lRegenta <- gsub("\\_", "", lRegenta) # italized
head(lRegenta)
lRegenta <- gsub("D\\.", "Doctor ", lRegenta) # D. to Doctor
lRegenta <- gsub("B\\.", "Benito", lRegenta) # B. to Benito
head(lRegenta)
lRegenta <- gsub("\\-\\-([A-Z]+)\\-\\-", "Capitulo \\1\\.", lRegenta) # Change chapters to have dot.
lRegenta <- gsub("(Tomo [A-Z]+)", "\\1.", lRegenta) # Change chapters to have dot.
lRegenta
lRegenta <- gsub("\\d\\K\\.(?=\\d)", "", lRegenta, perl = TRUE)#  Remove dots in thousands

# Convert to 1 string
texto <- paste(lRegenta, collapse = ' ')
length(texto)

# Function for splitting into sentences
vector <- c()
for (i in 1:length(texto)) {
  temp<-(strsplit(texto[[i]], "\\.")[[1]])
  print(temp)
  vector <- c(vector,temp)
}

# Transform vector to dataframe
df_regenta <- as.data.frame(vector)

colnames(df_regenta)[1] <- "sentence"

# Remove additional spaces on the left
df_regenta$sentence <- trimws(df_regenta$sentence, "l")

#Convert to character
df_regenta$sentence <- as.character(df_regenta$sentence)


# Create lexicon with stopwords
lexicon <- stopwords("es")
lexicon <- append(lexicon, c("capitulo", "tomo", " "))
lexicon <- as.data.frame(lexicon)
colnames(lexicon) <- c("word")
lexicon$word <- as.character(lexicon$word)


# Assign ID to each sentence
df_id <- tibble::rowid_to_column(df_regenta, "ID")


review_words <- df_id %>%
  distinct(sentence, .keep_all = TRUE) %>% # remove repeated sentences
  unnest_tokens(word, sentence, drop = FALSE) %>%
  distinct(ID, word, .keep_all = TRUE) %>%
  anti_join(lexicon) %>% # dont use lexicon words
  group_by(word) %>% # Group By word
  dplyr::mutate(word_total = n()) %>%
  ungroup()

# Count words
word_counts <- review_words %>%
  dplyr::count(word, sort = TRUE)

word_counts %>%
  head(40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "green") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = paste0("Most used words"),
       subtitle = "Removed Stopwords",
       x = "Word",
       y = "Times")

# WordCloud
library(wordcloud)
library(RColorBrewer)

df_groupedWords <- review_words %>% group_by(word) %>% count(word) %>%  
  group_by(word) %>% mutate(frequency = n/dim(review_words)[1])


# Generate wordcloud
wordcloud(words = df_groupedWords$word, freq = df_groupedWords$frequency,
          max.words = 400, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))


# 1.2. Bigrams
# Pair of words
review_bigrams <- df_id %>%
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2) # split into two

bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") # split words of bigrams

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% lexicon$word) %>%
  filter(!word2 %in% lexicon$word) # remove stopwords

bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE) # count bigrams

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") # count bigrams cleaning

bigrams_united %>%
  dplyr::count(bigram, sort = TRUE)


# PLOT BIGRAMS
bigrams_plot <- df_id %>% 
  unnest_tokens(word, sentence) %>% 
  anti_join(lexicon)
my_stopwords <- tibble(word = c(as.character(1:10)))
bigrams_plot <- bigrams_plot %>% 
  anti_join(my_stopwords)
title_word_pairs <- bigrams_plot %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)

bigramsList<-title_word_pairs[which(title_word_pairs$n>100),]
set.seed(1234)
title_word_pairs %>%
  filter(n >= 75) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Bigrams')


#Sentiment
library("syuzhet")
library("tm")

# split by words the text that is a huge string
text_words <- get_tokens(texto)

length(text_words) #308.304 words

text_sentences <- get_sentences(texto)

length(text_sentences)
# sentiment_vector uses sentences
sentiment_vector <- get_sentiment(text_sentences)

plot(
  sentiment_vector, 
  type="l", 
  main="Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

percent_vals <- get_percentage_values(sentiment_vector, bins = 15)
plot(
  percent_vals, 
  type="l", 
  main="Percentage-Based", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="purple"
)

# Real sentiments
df_sentiments <- get_nrc_sentiment(text_words, lang="spanish") # 40 minutes 

head(df_sentiments, 100)

summary(df_sentiments)

barplot(
  colSums(prop.table(df_sentiments[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "'La Regenta' de Benito Pérez Galdós",
  xlab="Sentiments", ylab = NULL)

barplot(colSums(prop.table(df_sentiments[, 1:8])))

sentiments_plot <- (df_sentiments$negative *-1) + df_sentiments$positive

simple_plot(sentiments_plot)


