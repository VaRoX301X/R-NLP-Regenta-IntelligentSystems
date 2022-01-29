
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
