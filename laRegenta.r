
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

