library(rtweet)
library(tidytext)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(gganimate)

create_token(
  app = "#100papers",
  consumer_key = "KluzVupHoqCx1c4WkP984913N",
  consumer_secret = "paTdg0ognq52pwOZ3563fuEKHRxvO8c1fSWXGhjKPOXgWWsQ05"
)


# get my last 3000 tweets

emily <- get_timeline(user = "emilynordmann", n = 3000)

# filter out tweets to the #100papers hashtag

dat100papers <- emily %>%
  select(created_at,hashtags,text, reply_to_user_id)%>%
  filter(hashtags == "100papers",
         reply_to_user_id == 1464887040)%>%
  mutate(month = as.factor(month(created_at)),
         day = day(created_at),
         weekday = as.factor(wday(created_at)),
         week = week(created_at))


# get rid of numbers, links, and the hashtags

dat <- dat100papers %>%
  mutate(text = str_replace_all(text, "[^\x01-\x7F]", ""),
         text = str_replace_all(text, "#100papers", ""),
         text = str_replace_all(text, "Journal", ""),
         text = str_replace_all(text, "\\.|[[:digit:]]+", ""),
         text = str_replace_all(text, "https|amp|tco", ""))


# plot of the counts of most frequent words

dat %>%  
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = str_to_title(word),
         word = reorder(word, n))%>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip()+
  scale_y_continuous(name = "Frequency")+
  scale_x_discrete(name = "#100papers words in titles")



# time series plot

ts_plot(dat, by = "3.5 weeks", lwd = 2)+
  theme(axis.text.x= element_text(angle = 90))+
  scale_x_datetime(date_breaks = "1 month",
                   labels = c("Dec", "Jan", "Feb", "March", "April", "May", "June", "July", "August", 
                              "Sept", "Oct", "Nov", "Dec", "Jan", "Feb"),
                   limits = c(
                     as.POSIXct("2018-01-01 00:00:00 GMT"),
                     as.POSIXct("2018-12-25 23:59:59 GMT"))
  )


# number of papers by day

dat %>%
  group_by(weekday)%>%
  summarise(n = n())%>%
  ggplot(aes(x = weekday, y = n, fill = weekday))+
  geom_col(show.legend = FALSE)+
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7),
                   labels = c("Monday", "Tuesday", "Wednesday", 
                              "Thursday", "Friday", "Saturday", "Sunday"),
                   name = "Day of week")+
  scale_y_continuous(name = "Number of papers read")


# number of papers by month

dat %>%
  group_by(month)%>%
  summarise(n = n())%>%
  ggplot(aes(x = month, y = n, fill = month))+
  geom_col(show.legend = FALSE)+
  annotate("text", label = "Semester 2 \n teaching", x = 2.5, y = 5, color = "Black", fontface = "bold")+
  annotate("text", label = "Summer!", x = 6.5, y = 11, color = "Black", fontface = "bold")+
  annotate("text", 
           label = "Semester 1 \n begins", x = 9.0, y = 3, color = "Black", size = 4, 
           fontface = "bold")+
  annotate("text", 
           label = "Teaching/admin \n wrapped up", x = 10.0, y = 12, color = "Black", size = 4, 
           fontface = "bold")+
  annotate("text", 
           label = "Start new \n job", x = 10.85, y = 7.5, color = "Black", size = 4, 
           fontface = "bold")+
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels = c("Jan", "Feb", "March", "April", "May", "June", "July", "August", "Sept",
                              "Oct", "Nov", "Dec"),
                   name = "Month")+
  scale_y_continuous(name = "Number of papers read")



