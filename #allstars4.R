library(rtweet)
library(tidytext)
library(tidyverse)
library(gridExtra)


# get tweets with the #allstars4 hashtag

tweets <- search_tweets(
  "#AllStars4", n = 18000, include_rts = FALSE
)

# Or read in the data file if you want to reproduce exactly

tweets <- read_csv("#AllStars4 tweets 2019-01-02.csv") %>%
  select(-X1)

queens <- c("manila", "trinity", "valentina", "naomi", "monique", "latrice","monet", "gia", "farrah", "jasmine")
remaining_queens <- c("manila", "trinity", "valentina", "naomi", "monique", "latrice","monet")

# get rid of numbers, links, and the hashtags then select timestamp and text columns

dat <- tweets %>%
  mutate(text = str_replace_all(text, "[^\x01-\x7F]", ""),
         text = str_replace_all(text, "#AllStars4", ""),
         text = str_replace_all(text, "AllStars", ""),
         text = str_replace_all(text, "allstars", ""),
         text = str_replace_all(text, "\\.|[[:digit:]]+", ""),
         text = str_replace_all(text, "https|amp|tco", ""))%>%
  select(created_at, text)%>%
  mutate(tweet = row_number())

# write.csv(x = dat, file = "#AllStars4 tweets 2019-01-02.csv")

# create tidy text

dat_token <- dat %>%  
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# convert tidy text back to wide so that all in lower case etc.

dat_lower <- dat_token %>%
  group_by(tweet) %>%
  summarise(text = str_c(word, collapse = " "))

# add in column to say if each queen mentioned in tweet

dat_lower <- dat_lower %>% 
  mutate(monet = case_when(str_detect(text, ".monet") ~ TRUE, TRUE ~ FALSE),
         latrice = case_when(str_detect(text, ".latrice") ~ TRUE, TRUE ~ FALSE),
         trinity = case_when(str_detect(text, ".trinity") ~ TRUE, TRUE ~ FALSE),
         farrah = case_when(str_detect(text, "farrah") ~ TRUE, TRUE ~ FALSE),
         valentina = case_when(str_detect(text, ".valentina") ~ TRUE, TRUE ~ FALSE),
         monique = case_when(str_detect(text, ".monique") ~ TRUE, TRUE ~ FALSE),
         jasmine = case_when(str_detect(text, ".jasmine") ~ TRUE, TRUE ~ FALSE),
         naomi = case_when(str_detect(text, ".naomi") ~ TRUE, TRUE ~ FALSE),
         gia = case_when(str_detect(text, ".gia") ~ TRUE, TRUE ~ FALSE),
         manila = case_when(str_detect(text, ".manila") ~ TRUE, 
                            str_detect(text, ".manilla") ~ TRUE,
                            TRUE ~ FALSE)
  )


# create tidy text with the mention columns

dat_mentions <- dat_lower %>%  
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")


# create time series plot

ts_plot(dat, "2 hours")+
  theme_minimal()


# plot of the counts of most frequent names

dat_token %>%
  filter(word %in% c("monet", "latrice", "trinity", "farrah", "valentina", "monique", "jasmine",
                     "naomi", "gia", "manila"))%>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = str_to_title(word),
         word = reorder(word, n))%>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip()+
  scale_y_continuous(name = "Number of tweets")+
  scale_x_discrete(name = "Queens")

# do a sentiment analysis for each queen

gia <- dat_mentions %>%
  filter(gia == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "gia")

valentina <- dat_mentions %>%
  filter(valentina == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "valentina")

manila <- dat_mentions %>%
  filter(manila == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "manila")

trinity <- dat_mentions %>%
  filter(trinity == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "trinity")

naomi <- dat_mentions %>%
  filter(naomi == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "naomi")

latrice <- dat_mentions %>%
  filter(latrice == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "latrice")

monet <- dat_mentions %>%
  filter(monet == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "monet")

monique <- dat_mentions %>%
  filter(monique == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "monique")

farrah <- dat_mentions %>%
  filter(farrah == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "farrah")

jasmine <- dat_mentions %>%
  filter(jasmine == "TRUE")%>%
  inner_join(get_sentiments("bing"))%>%
  count(index = tweet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(queen = "jasmine")

# combine sentiment analysis for each queen into one tibble and then calculate total positive, negative
# and overall sentimment scores for each queen

dat_sentiment <- bind_rows(gia,valentina,manila,trinity,naomi,monique,latrice,monet,farrah,jasmine) %>%
  group_by(queen) %>%
  summarise(positive = sum(positive),
            negative = sum(negative),
            overall = sum(sentiment))%>%
  gather(positive:overall, key = type, value = score)%>%
  mutate(type = factor(type, levels = c("positive", "negative", "overall")))%>%
  mutate(queen = factor(queen, levels = c("manila", "trinity", "valentina", "naomi", "monique",
                                          "latrice","jasmine", "monet", "farrah", "gia")))

# display table of sentiment scores

tbl <- dat_sentiment %>%
  spread(type, score)%>%
  arrange(desc(overall))

grid.table(tbl)


# create plot of the sentiment scores by each queen, ordered by overall score

ggplot(dat_sentiment, aes(x = type, y = score, fill = type)) +
  stat_identity(geom = "bar", position = "dodge", show.legend = FALSE)+
  facet_wrap(~ queen, ncol = 3)+
  coord_flip()

# create plot of the sentiment scores by each queen, but exclude eliminated queens

dat_sentiment%>%
  filter(queen %in% remaining_queens) %>%
           ggplot(aes(x = type, y = score, fill = type)) +
           stat_identity(geom = "bar", position = "dodge", show.legend = FALSE)+
           facet_wrap(~ queen, ncol = 3)+
           coord_flip()
