
# Load Data
pacman::p_load(rtweet, quanteda, text2vec, tidyverse, quanteda.textplots,
               quanteda.textstats, lubridate, ggpubr, quanteda.textmodels,
               seededlda, stm, ldatuning, broom, tidytext, caret)

df <- readRDS("twitter_df.RDS")
tweets <- select(df, screen_name, text, created_at)

# tweets <- tweets %>%
#   filter(as.Date(created_at)<"2022-01-01")

zmd <- get_timeline("der_zmd", n= 3200)
alhm <- get_timeline("Alhambra_eV", n= 3200)
ditib <- get_timeline("DITIBkoln", n=3200)
igmggenclik <- get_timeline("igmggenclik", n=3200)
islamratbrd <- get_timeline("Islamratbrd", n=3200)

# zmd <- zmd %>%
#   filter(as.Date(created_at)<"2022-01-01")
corp_tweets <- corpus(df)
# Descriptive Data----
ts <- tweets
ts$tweet <- 1L

ts <- ts %>%
  mutate(monthly = format_ISO8601(created_at, precision = "ym")) %>%
  mutate(weekly = str_c(
    formatC(isoweek(created_at), format = "f", digits = 0, width = 2, flag = "0"), 
    "/", 
    str_sub(isoyear(created_at), 3, 4))) %>%
  group_by(screen_name) %>%
  arrange(created_at, .by_group = TRUE) %>%
  mutate("cum" = cumsum(tweet))

ts <- ts %>%
  group_by(screen_name, monthly) %>%
  mutate("month_cum" = cumsum(tweet))

ts <- ts %>%
  group_by(screen_name, weekly) %>%
  mutate("week_cum" = cumsum(tweet)) 

cum_tweet <- ggplot(ts) +
  geom_line(aes(x=as.Date(created_at), y=cum, color=screen_name)) +
  # geom_point(aes(x=as.Date(created_at), y=week_cum*10, color=screen_name), alpha = 0.1) +
  xlab("Datum") +
  ylab("Tweets (kumulativ)") +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(color="Account") +
  scale_color_brewer(palette="Dark2")


week_tweet <- ggplot(ts) +
  geom_point(aes(x=as.Date(created_at), y=week_cum, color=screen_name), alpha = 0.1) +
  geom_smooth(aes(x=as.Date(created_at), y=week_cum, color=screen_name), se = FALSE) +
  xlab("Datum") +
  ylab("Tweets (wöchentlich)") +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(color="Account") +
  scale_color_brewer(palette="Dark2") +
  facet_wrap(~ screen_name)

ggarrange(cum_tweet, week_tweet, ncol = 1, nrow = 2, align = "v")

png(file="./grph_analyse/tweets-by-time.png", width = 1000, height = 549)
ggarrange(cum_tweet, week_tweet, ncol = 1, nrow = 2, align = "v")
dev.off()

svg(file="./grph_analyse/tweets-by-time.svg", width = 1000, height = 549)
ggarrange(cum_tweet, week_tweet, ncol = 1, nrow = 2, align = "v")
dev.off()

bmp(file="./grph_analyse/tweets-by-time.bmp", width = 1000, height = 549)
ggarrange(cum_tweet, week_tweet, ncol = 1, nrow = 2, align = "v")
dev.off()


# Text as Data----
## Pre-Processing====

dfm_pre <- corpus_subset(corp_tweets, 
                          screen_name %in% c("islam_realitaet", "genislam1", "MInteraktiv")) %>%
  dfm() %>%
  dfm_group(groups = screen_name)

dfm_post <- corpus_subset(corp_tweets, 
                         screen_name %in% c("islam_realitaet", "genislam1", "MInteraktiv")) %>%
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T, 
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_select(pattern=stp,selection='remove') %>%
  dfm() %>%
  dfm_group(groups = screen_name) %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de")) %>%
  dfm_trim(max_termfreq = .99,termfreq_type = "quantile",verbose = T) %>%
  dfm_trim(min_termfreq = .7,termfreq_type = "quantile",verbose = T) %>%
  dfm_wordstem()


pre_pp <- textstat_frequency(dfm_pre, n = 50)

pre_pp$feature <- with(pre_pp, reorder(feature, -frequency))

pp_1 <- ggplot(pre_pp, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Häufigkeit") +
  xlab("Textelemente (Ohne Vorverarbeitung)")

post_pp <- textstat_frequency(dfm_post, n = 50)

post_pp$feature <- with(post_pp, reorder(feature, -frequency))

pp_2 <- ggplot(post_pp, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Häufigkeit") +
  xlab("Textelemente (Mit Vorverarbeitung)")

ggarrange(pp_1, pp_2, ncol = 1, nrow = 2, align = "v")

png(file="./grph_analyse/dfm_freq.png", width = 994, height = 546)
ggarrange(pp_1, pp_2, ncol = 1, nrow = 2, align = "v")
dev.off()

svg(file="./grph_analyse/dfm_freq.svg", width = 994, height = 546)
ggarrange(pp_1, pp_2, ncol = 1, nrow = 2, align = "v")
dev.off()

bmp(file="./grph_analyse/dfm_freq.bmp", width = 994, height = 546)
ggarrange(pp_1, pp_2, ncol = 1, nrow = 2, align = "v")
dev.off()


## Wordclouds====
### By Account

set.seed(100)
wordclout_1 <- corpus_subset(corp_tweets, 
              screen_name %in% c("islam_realitaet", "genislam1", "MInteraktiv")) %>%
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T, 
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_select(pattern=stp,selection='remove') %>%
  dfm() %>%
  dfm_group(groups = screen_name) %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de")) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE)

set.seed(100)
png(file="./grph_analyse/wordclout1.png", width = 867, height = 478)
wordclout_1 %>%
  textplot_wordcloud(comparison = TRUE, max_words = 100)
dev.off()
set.seed(100)
svg(file="./grph_analyse/wordclout1.svg", width = 867, height = 478)
wordclout_1 %>%
  textplot_wordcloud(comparison = TRUE, max_words = 100)
dev.off()
set.seed(100)
bmp(file="./grph_analyse/wordclout1.bmp", width = 867, height = 478)
wordclout_1 %>%
  textplot_wordcloud(comparison = TRUE, max_words = 100)
dev.off()

### 2-gram
two_gram <- corpus_subset(corp_tweets, 
                       screen_name %in% c("islam_realitaet", "genislam1", "MInteraktiv")) %>%
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T, 
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_select(pattern=stp,selection='remove') %>%
  tokens_ngrams() %>%
  dfm() %>%
  dfm_group(groups = screen_name) %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de"))

set.seed(100)
two_gram %>%
  textplot_wordcloud(comparison = FALSE, max_words = 100)

set.seed(100)
png(file="./grph_analyse/two_grampn.png", width = 870, height = 478)
two_gram %>%
  textplot_wordcloud(comparison = FALSE, max_words = 100)
dev.off()
set.seed(100)
svg(file="./grph_analyse/two_gram.svg", width = 870, height = 478)
two_gram %>%
  textplot_wordcloud(comparison = FALSE, max_words = 100)
dev.off()
set.seed(100)
bmp(file="./grph_analyse/two_gram.bmp", width = 870, height = 478)
two_gram %>%
  textplot_wordcloud(comparison = FALSE, max_words = 100)
dev.off()

### 3-gram
three_gram <- corpus_subset(corp_tweets, 
                          screen_name %in% c("islam_realitaet", "genislam1", "MInteraktiv")) %>%
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T, 
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_select(pattern=stp,selection='remove') %>%
  tokens_ngrams(n=3) %>%
  dfm() %>%
  dfm_group(groups = screen_name) %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de"))

set.seed(100)
three_gram %>%
  textplot_wordcloud(comparison = FALSE, max_words = 100)

set.seed(100)
png(file="./grph_analyse/three_gram.png", width = 870, height = 478)
three_gram %>%
  textplot_wordcloud(comparison = FALSE, max_words = 100)
dev.off()
set.seed(100)
svg(file="./grph_analyse/three_gram.svg", width = 870, height = 478)
three_gram %>%
  textplot_wordcloud(comparison = FALSE, max_words = 100)
dev.off()
set.seed(100)
bmp(file="./grph_analyse/three_gram.bmp", width = 870, height = 478)
three_gram %>%
  textplot_wordcloud(comparison = FALSE, max_words = 100)
dev.off()

## Keyness ====

key_df <- bind_rows(filter(df, screen_name=="genislam1"), zmd)

key_corp <- corpus(key_df)

key_dfm <- tokens(key_corp, remove_punct=T,
                  remove_numbers = T,
                  remove_url = T,
                  split_hyphens = T,
                  remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_select(pattern=stp,selection='remove') %>%
  dfm() %>%
  dfm_group(groups = screen_name) %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de")) %>%
  dfm_trim(max_termfreq = .99,termfreq_type = "quantile",verbose = T) %>%
  dfm_trim(min_termfreq = .7,termfreq_type = "quantile",verbose = T)

result_keyness <- textstat_keyness(key_dfm, target = "genislam1")
textplot_keyness(result_keyness)
  

png(file="./grph_analyse/keyness.png", width = 1050, height = 856)
textplot_keyness(result_keyness)
dev.off()

svg(file="./grph_analyse/keyness.svg", width = 1050, height = 856)
textplot_keyness(result_keyness)
dev.off()

bmp(file="./grph_analyse/keyness.bmp", width = 1050, height = 856)
textplot_keyness(result_keyness)
dev.off()

# Unsupervised Learning - Topic Modelling----
## Regular/STM ====
arab <- c("ا", "ب", "ت", "ث", "ج", "ح", "خ"
  , "د", "ذ", "ر", "ز", "س", "ش", "ص", "ض", "ط", "ظ", "ع",
  "غ", "ف", "ق", "ك", "ل", "م", "ن", "ه", "و", "ي", "ء")
dfm_post <- corpus_subset(corp_tweets, 
                          screen_name %in% c("islam_realitaet", "genislam1", "MInteraktiv")) %>%
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T, 
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_remove(stopwords("german", source = "stopwords-iso")) %>%
  tokens_remove(stopwords("french")) %>%
  tokens_remove(stopwords(language = "ar", source = "stopwords-iso")) %>%
  tokens_select(pattern=arab, selection='remove') %>%
  dfm() %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de")) %>%
  dfm_trim(max_termfreq = .99,termfreq_type = "quantile",verbose = T) %>%
  dfm_trim(min_termfreq = .7,termfreq_type = "quantile",verbose = T) %>%
  dfm_wordstem()

result <- FindTopicsNumber(
  dfm_post,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed =1234),
  mc.cores = NA,
  verbose = TRUE)



lda_plot(result)

png(file="./grph_analyse/lda_tune.png", width = 1045, height = 575)
lda_plot(result)
dev.off()

svg(file="./grph_analyse/lda_tune.svg", width = 1045, height = 575)
lda_plot(result)
dev.off()

bmp(file="./grph_analyse/lda_tune.bmp", width = 1045, height = 575)
lda_plot(result)
dev.off()

set.seed(1234)
tmod_lda <- textmodel_lda(dfm_post, k = 10)
terms(tmod_lda, 10)

topic.count <- 10
dfm2stm <- convert(dfm_post, to = "stm")
model.stm <- stm(dfm2stm$documents, dfm2stm$vocab, K = topic.count, data = dfm2stm$meta, init.type = "Spectral")

plot(model.stm, type = "summary", text.cex = 1)

doc_probs <- tidy(model.stm, matrix = "gamma", document_names = model.stm$meta$title)
terms <- labelTopics(model.stm)
top_terms <- tibble(topic = terms$topicnums,
                    prob = apply(terms$prob, 1, paste, collapse = ", "),
                    frex = apply(terms$frex, 1, paste, collapse = ", "))
doc_probs
terms
top_terms

gamma_by_topic <- doc_probs %>% 
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))


toptopics <- gamma_by_topic %>% 
  ggplot(aes(topic, gamma, label = frex, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25), labels = scales::percent) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  labs(x = NULL, y = expression(gamma)) +
  scale_fill_brewer(palette="BrBG")


png(file="./grph_analyse/toptopics.png", width = 1045, height = 575)
toptopics
dev.off()

svg(file="./grph_analyse/toptopics.svg", width = 1045, height = 575)
toptopics
dev.off()

bmp(file="./grph_analyse/toptopics.bmp", width = 1045, height = 575)
toptopics
dev.off()

set.seed(100)
plot(model.stm, type = "perspectives", topics = c(3,5))

png(file="./grph_analyse/toptopics.png", width = 1333 , height = 734)
set.seed(100)
plot(model.stm, type = "perspectives", topics = c(3,5))
dev.off()

svg(file="./grph_analyse/perspectives.svg", width = 1333 , height = 734)
set.seed(100)
plot(model.stm, type = "perspectives", topics = c(3,5))
dev.off()

bmp(file="./grph_analyse/perspectives.bmp", width = 1333 , height = 734)
set.seed(100)
plot(model.stm, type = "perspectives", topics = c(3,5))
dev.off()

## Topics by Time
model.stm.labels <- labelTopics(model.stm, 1:topic.count)
dfm2stm$meta$created_at_2 <- as.numeric(format(as.Date(dfm2stm$meta$created_at),'%Y'))
model.stm.ee <- estimateEffect(1:topic.count ~ s(created_at_2), model.stm, meta = dfm2stm$meta)


png(file="./grph_analyse/time_stm.png", width = 1411 , height = 774)
par(mfrow=c(4,3))
for (i in seq_along(sample(1:topic.count, size = 10)))
{
  plot(model.stm.ee, "created_at_2", method = "continuous", topics = i, main = paste0(model.stm.labels$prob[i,1:3], collapse = ", "), printlegend = F)
  # yearseq <- seq(from = ymd("2019-07-22"),
  #                to = ymd("2022-04-19"), by = "day")
  # daynames <- months(yearseq)
  # axis(1,at = as.numeric(yearseq) - min(as.numeric(yearseq)),
  #      labels = daynames)
}
dev.off()

svg(file="./grph_analyse/time_stm.svg", width = 1411 , height = 774)
par(mfrow=c(4,3))
for (i in seq_along(sample(1:topic.count, size = 10)))
{
  plot(model.stm.ee, "created_at_2", method = "continuous", topics = i, main = paste0(model.stm.labels$prob[i,1:3], collapse = ", "), printlegend = F)
  # yearseq <- seq(from = ymd("2019-07-22"),
  #                to = ymd("2022-04-19"), by = "day")
  # daynames <- months(yearseq)
  # axis(1,at = as.numeric(yearseq) - min(as.numeric(yearseq)),
  #      labels = daynames)
}
dev.off()

bmp(file="./grph_analyse/time_stm.svg.bmp", width = 1411 , height = 774)
par(mfrow=c(4,3))
for (i in seq_along(sample(1:topic.count, size = 10)))
{
  plot(model.stm.ee, "created_at_2", method = "continuous", topics = i, main = paste0(model.stm.labels$prob[i,1:3], collapse = ", "), printlegend = F)
  # yearseq <- seq(from = ymd("2019-07-22"),
  #                to = ymd("2022-04-19"), by = "day")
  # daynames <- months(yearseq)
  # axis(1,at = as.numeric(yearseq) - min(as.numeric(yearseq)),
  #      labels = daynames)
}
dev.off()

## Seeded Topics (palestine/israel, uiguirs/china, kopftuch, Afd/cdu, etc.)====
dict <- dictionary(list(middle_east = c("israel*", "palest*", "paläs*", 
                                        "jerusalem", "aqsa", "*bank", "gaza", 
                                        "aparth*", "antisemit*"),
                        muslim_opression  = c("china", "chines*", "uigur*", "uyghur*", "chines*", "camp*", "hindu*", "myanm*", "rohingy*"),
                        resist = c("verteid*", "resist*", "defen*", "widerstand*", "battl*", "wehren*", "abwehr*", "oppos*"),
                        kopftuch = c("kopftuch*", "scarf*", "niqab*", "jilbab*", "hijab*"),
                        racism = c("rassis*", "race*", "racis*", "black*", "white*", "discrim*", "diskrim*", "nazi*"),
                        islam = c("dua", "quran", "koran", "hadith", "narration", "prophet", "gesandter", 
                                   "rasul*", "sahaba*", "salaf", "kalif*", "mubarak", "eid", "deen")))

dfm_seed <- corpus_subset(corp_tweets, 
                          screen_name %in% c("islam_realitaet", "genislam1", "MInteraktiv")) %>%
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T, 
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_remove(stopwords("german", source = "stopwords-iso")) %>%
  tokens_remove(stopwords(language = "ar", source = "stopwords-iso")) %>%
  tokens_select(pattern=arab, selection='remove') %>%
  dfm() %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de", "à", "la")) %>%
  dfm_trim(max_termfreq = .99,termfreq_type = "quantile",verbose = T) %>%
  dfm_trim(min_termfreq = .7,termfreq_type = "quantile",verbose = T) %>%
  dfm_wordstem()


tmod_slda <- textmodel_seededlda(dfm_seed, dictionary = dict)

terms(tmod_slda, 30)


dfm_seed$topic2 <- topics(tmod_slda)
table(dfm_seed$topic2)

# Supervised Learning----
## Klein====
key_df_small<- key_df
key_df_small<- key_df_small%>%
  dplyr::filter(screen_name %in% c("genislam1", "der_zmd")) %>%
  dplyr::select(text, screen_name)

data_corpus <- corpus(key_df_small, text_field = "text")
# Set a seed for replication purposes
set.seed(68159)

# Generate random 10,000 numbers without replacement
training_id <- sample(1:4666, 2333, replace = FALSE)

# Create docvar with ID
docvars(data_corpus, "id_numeric") <- 1:ndoc(data_corpus)

# Get training set
dfmat_training <-
  corpus_subset(data_corpus, id_numeric %in% training_id) %>%
  dfm(stem = TRUE)
ke
# Get test set (documents not in training_id)
dfmat_test <-
  corpus_subset(data_corpus,!id_numeric %in% training_id) %>%
  dfm(stem = TRUE)

print(prop.table(table(docvars(
  dfmat_training, "screen_name"
))) * 100)

print(prop.table(table(docvars(
  dfmat_test, "screen_name"
))) * 100)


# Train naive Bayes
# The function takes a DFM as the first argument 
model.NB <- textmodel_nb(dfmat_training, docvars(dfmat_training, "screen_name"), prior = "docfreq")

# The prior indicates an assumed distribution. 
# Here we choose how frequently the categories occur in our data.

dfmat_matched <-
  dfm_match(dfmat_test, features = featnames(dfmat_training))

summary(model.NB)


prop.table(table(predict(model.NB) == docvars(dfmat_training, "screen_name"))) * 100
prop.table(table(sample(predict(model.NB)) == docvars(dfmat_training, "screen_name"))) * 100


actual_class <- docvars(dfmat_matched, "screen_name")
predicted_class <- predict(model.NB, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

confusion <- confusionMatrix(tab_class, mode = "everything")
confusion


confusion.data <- as.data.frame(confusion[["table"]])

# Reverse the order
level_order_y <-
  factor(confusion.data$actual_class,
         level = c("genislam1", "der_zmd"))

ggplot(confusion.data,
       aes(x = predicted_class, y = level_order_y, fill = Freq)) +
  xlab("Vorhergesagt") +
  ylab("Tatsächlich") +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) 

## Groß====
key_df_big <- bind_rows(key_df, alhm, ditib, igmggenclik, islamratbrd)
key_df_big <- key_df_big %>%
  dplyr::filter(screen_name %in% c("genislam1", "der_zmd", "Alhambra_eV",
                                   "DITIBkoln", "igmggenclik", "Islamratbrd")) %>%
  dplyr::select(text, screen_name)

data_corpus <- corpus(key_df_big, text_field = "text")
# Set a seed for replication purposes
set.seed(68159)

# Generate random 10,000 numbers without replacement
training_id <- sample(1:30000, 10000, replace = FALSE)

# Create docvar with ID
docvars(data_corpus, "id_numeric") <- 1:ndoc(data_corpus)

# Get training set
dfmat_training <-
  corpus_subset(data_corpus, id_numeric %in% training_id) %>%
  dfm(stem = TRUE)

# Get test set (documents not in training_id)
dfmat_test <-
  corpus_subset(data_corpus,!id_numeric %in% training_id) %>%
  dfm(stem = TRUE)

print(prop.table(table(docvars(
  dfmat_training, "screen_name"
))) * 100)

print(prop.table(table(docvars(
  dfmat_test, "screen_name"
))) * 100)


# Train naive Bayes
# The function takes a DFM as the first argument 
model.NB <- textmodel_nb(dfmat_training, docvars(dfmat_training, "screen_name"), prior = "docfreq")

# The prior indicates an assumed distribution. 
# Here we choose how frequently the categories occur in our data.

dfmat_matched <-
  dfm_match(dfmat_test, features = featnames(dfmat_training))

summary(model.NB)


prop.table(table(predict(model.NB) == docvars(dfmat_training, "screen_name"))) * 100
prop.table(table(sample(predict(model.NB)) == docvars(dfmat_training, "screen_name"))) * 100


actual_class <- docvars(dfmat_matched, "screen_name")
predicted_class <- predict(model.NB, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

confusion <- confusionMatrix(tab_class, mode = "everything")
confusion


confusion.data <- as.data.frame(confusion[["table"]])

# Reverse the order
level_order_y <-
  factor(confusion.data$actual_class,
         level = c("genislam1", "der_zmd"))

ggplot(confusion.data,
       aes(x = predicted_class, y = level_order_y, fill = Freq)) +
  xlab("Vorhergesagt") +
  ylab("Tatsächlich") +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) 

## Groß====
key_df_big <- bind_rows(key_df, alhm, ditib, igmggenclik, islamratbrd)
key_df_big <- key_df_big %>%
  dplyr::filter(screen_name %in% c("genislam1", "der_zmd", "Alhambra_eV",
                                   "DITIBkoln", "igmggenclik", "Islamratbrd")) %>%
  dplyr::select(text, screen_name)

data_corpus <- corpus(key_df_big, text_field = "text")
# Set a seed for replication purposes
set.seed(68159)

# Generate random 10,000 numbers without replacement
training_id <- sample(1:11425, 1904, replace = FALSE)

# Create docvar with ID
docvars(data_corpus, "id_numeric") <- 1:ndoc(data_corpus)

# Get training set
dfmat_training <-
  corpus_subset(data_corpus, id_numeric %in% training_id) %>%
  dfm(stem = TRUE)

# Get test set (documents not in training_id)
dfmat_test <-
  corpus_subset(data_corpus,!id_numeric %in% training_id) %>%
  dfm(stem = TRUE)

print(prop.table(table(docvars(
  dfmat_training, "screen_name"
))) * 100)

print(prop.table(table(docvars(
  dfmat_test, "screen_name"
))) * 100)


# Train naive Bayes
# The function takes a DFM as the first argument 
model.NB <- textmodel_nb(dfmat_training, docvars(dfmat_training, "screen_name"), prior = "docfreq")

# The prior indicates an assumed distribution. 
# Here we choose how frequently the categories occur in our data.

dfmat_matched <-
  dfm_match(dfmat_test, features = featnames(dfmat_training))

summary(model.NB)


prop.table(table(predict(model.NB) == docvars(dfmat_training, "screen_name"))) * 100
prop.table(table(sample(predict(model.NB)) == docvars(dfmat_training, "screen_name"))) * 100


actual_class <- docvars(dfmat_matched, "screen_name")
predicted_class <- predict(model.NB, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

confusion <- confusionMatrix(tab_class, mode = "everything")
confusion


confusion.data <- as.data.frame(confusion[["table"]])

# Reverse the order
level_order_y <-
  factor(confusion.data$actual_class,
         level = c("genislam1", "der_zmd", "Alhambra_eV",
                   "DITIBkoln", "igmggenclik", "Islamratbrd"))

ggplot(confusion.data,
       aes(x = predicted_class, y = level_order_y, fill = Freq)) +
  xlab("Vorhergesagt") +
  ylab("Tatsächlich") +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) 

