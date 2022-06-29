pacman::p_load(quanteda, text2vec, tidyverse)


df <- readRDS("twitter_df.RDS")
tweets <- select(df, screen_name, text, created_at)

## Heranarbeiten ---------------------------------
## Tokenize = each document several tokens meaning words
ft <- tokens(df$text)
ft[10]

## DFM - Document Feature Matrix. Rows = Individuals = Documents = Tweets
## COlumns = Variables = Words. Rectangle shape!
dfc<-dfm(ft)
dfc

## Word freq
scores<-topfeatures(dfc,40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores,
        horiz=TRUE,las=1,cex.axis =.2)

## Pre-processing
ft <- tokens(df$text,remove_punct=T,
           remove_numbers = T,
           remove_url = T, 
           split_hyphens = T,
           remove_symbols = T)


## Word freq vers. 2
scores<-topfeatures(dfm(ft),40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores,
        horiz=TRUE,las=1,cex.axis =.2)

## n_grams
#Bi-Grams
dfm_ng<-dfm(tokens_ngrams(ft))
dfm_ng

dfm_ng_3 <-dfm(tokens_ngrams(tokens(ft), n=3))
dfm_ng_3

## Word freq vers. 3
scores<-topfeatures(dfm(dfm_ng),40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores,
        horiz=TRUE,las=1,cex.axis =.2)

scores<-topfeatures(dfm(dfm_ng_3),40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores,
        horiz=TRUE,las=1,cex.axis =.2)

## Stemming
dfc_s<-dfm(tokens_wordstem(ft))
dfc_s


## Feature Destruction
ft<-tokens_tolower(ft)

ft<-tokens_select(ft,pattern=stopwords("en"),selection='remove')
ft<-tokens_select(ft,pattern=stopwords("de"),selection='remove')

## Stemming 2.0
dfc_s<-dfm(tokens_wordstem(ft))
dfc_s

## n_grams 2.0
#Bi-Grams
dfm_ng<-dfm(tokens_ngrams(ft))
dfm_ng

dfm_ng_3 <-dfm(tokens_ngrams(tokens(ft), n=3))
dfm_ng_3

## Word freq vers. 4
scores<-topfeatures(dfm(dfm_ng),40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores,
        horiz=TRUE,las=1,cex.axis =.2)

scores<-topfeatures(dfm(dfm_ng_3),40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores,
        horiz=TRUE,las=1,cex.axis =.2)
## ---------------------------------

### Schritte:
# Tokenization
# DFM
# Pre-Processing
  # Zeichen etc.
  # Stopwords
  # Remove Frequent Words (Trimming, Zipf's Law) 
  # Stemming (tokens_wordstem(chparl_de_left.toks, language = ("de")))
# Analysen
  # Term Frequencies
  # N-Grams
  # Dictionary Analysis
  # Word Embeddings


## Tokenize und Teile des Pre-Processings
ft <- tokens(df$text)
ft[8:10]
ft <- tokens(df$text,remove_punct=T,
             remove_numbers = T,
             remove_url = T, 
             split_hyphens = T,
             remove_symbols = T)
ft[8:10]
ft<-tokens_tolower(ft)
ft[8:10]

# Stopwords
stp <- stopwords("de", source = "stopwords-iso")
stp <- Filter(function(x) nchar(x) > 1, stp)
ft<-tokens_select(ft,pattern=stp,selection='remove')
ft[8:10]
ft<-tokens_select(ft,pattern=stopwords("en", source = "snowball"),selection='remove')
ft[8:10]

# N-Grams
dfm_ng_2<-dfm(tokens_ngrams(tokens(ft)))
dfm_ng_2

scores<-topfeatures(dfm_ng_2,40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores)

dfm_ng_3 <-dfm(tokens_ngrams(tokens(ft), n=3))

scores<-topfeatures(dfm_ng_3,40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores)

## DFM
dfc<-dfm(ft)
dfc

## Pre-Processing Teil 2
# Trimming
dfc<-dfm_trim(dfc,max_termfreq = .99,termfreq_type = "quantile",verbose = T)
dfc<-dfm_trim(dfc,min_termfreq = .7,termfreq_type = "quantile",verbose = T)
ft[8:10]

scores<-topfeatures(dfc,40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores)

# Stemming
dfc <- dfm_wordstem(dfc)

scores<-topfeatures(dfc,40)
scores<-scores[order(scores,decreasing =F)]
barplot(scores)

## Analysen
# Dictionary Analysis
# Word Embeddings
