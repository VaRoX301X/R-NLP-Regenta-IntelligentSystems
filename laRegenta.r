
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


