# load packages
library(tidyverse)
library(tidytext)
library(topicmodels)
library(dbscan)

# load data
load("data/wrangle/model_clean.rda")
load("data/wrangle/text_data.rda")

# LDA modeling ----
## DTM data
dtm <- token_counts %>%
  unite(document, c(program_name, index), sep = "_") %>%
  cast_dtm(term = word, document = document, value = n, weight = tm::weightTf)
  
## fit model 
lda <- LDA(dtm, k = 4, control = list(seed = 1234))

## per-document-per-topic probability
gamma <- tidy(lda, matrix = "gamma") %>%
  separate(document, c("program_name", "index"), sep = "_", convert = TRUE)

## classifications 
classifications <- gamma %>%
  group_by(program_name, index) %>%
  slice_max(gamma) %>%
  ungroup()

## plot?

# Latent Dirichlet Allocation (LDA): Assumes documents are mixtures of topics, and topics are mixtures of words.
# Non-Negative Matrix Factorization (NMF): Decomposes the document-term matrix into topics and term weights.
# Latent Semantic Analysis (LSA): Uses singular value decomposition to identify topics.

# HDBSCAN modeling ----
## fit model
hdbscan <- hdbscan(dtm, minPts = 5000)

# k-means modeling ----

