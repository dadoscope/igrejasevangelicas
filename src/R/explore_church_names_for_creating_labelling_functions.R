# DATASCOPE
# Get data and explore terms to create labelling functions
# for SnorkelML
# Author: Henrique Gomide.

library(tidyverse)
library(tm)

# Read data ---------------------------------------------------------------
churches <- read.csv(unzip("data/igreja_cnpj_ativo.zip"),
                     stringsAsFactors = FALSE,
                     encoding = "ISO-8859-1")

churches$razao_social <- iconv(churches$razao_social, "ISO-8859-1", "UTF-8")

# Get church names --------------------------------------------------------
church_names <- sort(unique(churches$razao_social))
church_names <- data.frame(church.name = church_names, stringsAsFactors = FALSE)

# Split data
set.seed(123) 

sample_size <- floor(.1 * length(church_names$church.name))
train_ind   <- sample(seq_len(length(church_names$church.name)), size = sample_size)
train_data <- data.frame(church.name = church_names[train_ind, ], stringsAsFactors = FALSE)


# Explore data
corpus <- VectorSource(train_data$church.name)
corpus <- VCorpus(corpus)

# Create Function to Clean Corpus
clearCorpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

corpus <- clearCorpus(corpus)
dtm <- TermDocumentMatrix(corpus)

dtm.matrix <- as.matrix(dtm)
rm(corpus, dtm)
dtm.matrix <- apply(dtm.matrix, 2, function(x) ifelse(x > 0, 1, 0))

term.freq <- rowSums(dtm.matrix)
temp.data <- data.frame(word = names(term.freq),
                        frequency = term.freq)

temp.data <- arrange(temp.data, 
                     desc(frequency))

# Explore Data Frame (e.g. below)
View(train_data %>% filter(grep("METODISTA", church.name)))