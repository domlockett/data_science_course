# 18/18


# question 1 -GOOD
library(ggplot2) # read in library
# read in data
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
# candidates of interest (from class)
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", 
                                                            "Bernard Sanders", 
                                                            "Elizabeth Warren", 
                                                            "Joseph R. Biden Jr.", 
                                                            "Michael Bloomberg", 
                                                            "Pete Buttigieg"),]
# states of interest (from class)
primaryPolls<-primaryPolls[primaryPolls$state=="Alabama" | 
                             primaryPolls$state=="Arkansas" | 
                             primaryPolls$state=="California"| 
                             primaryPolls$state=="Colorado"| 
                             primaryPolls$state=="Maine"| 
                             primaryPolls$state=="Massachusetts"| 
                             primaryPolls$state=="Minnesota"| 
                             primaryPolls$state=="North Carolina"| 
                             primaryPolls$state=="Oklahoma"| 
                             primaryPolls$state=="Tennessee"| 
                             primaryPolls$state=="Utah"| 
                             primaryPolls$state=="Vermont"| 
                             primaryPolls$state=="Virginia", ]
# maybe come back to this later
# primaryPolls_cal = primaryPolls[primaryPolls$state == "New Hampshire",]
ggplot(data=primaryPolls)+
  geom_point(mapping = aes(x=start_date, y=pct)) + 
  facet_wrap(~ candidate_name, nrow=2) +
  theme_minimal() +
  xlab("date") +
  ylab("percentage")


# quesiton 2 -GOOD
library(tidyverse)
primaryPolls_wide<-pivot_wider(primaryPolls, names_from = candidate_name, 
                               values_from = pct)
# the original dataset: 4001704 bytes
# the candidate-state dyad wide dataset: 13869768 bytes

# question 3
# reading in packages
library(fivethirtyeight)
library(tidyverse)

# reading in data
setwd("C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/problem_sets/ps3")
polls = read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')

# modifying data frames/tibbles
Endorsements <- endorsements_2020
Endorsements = Endorsements %>%
  rename(candidate_name = endorsee) %>%
  as_tibble()
polls = polls %>%
  select(candidate_name, sample_size, start_date, party, pct) %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))

# fix candidate names
unique(Endorsements$candidate_name)
Endorsements = Endorsements %>%
  mutate(candidate_name = recode(candidate_name, "Bernie Sanders" = "Bernard Sanders")) %>%
  mutate(candidate_name = recode(candidate_name, "Joe Biden" = "Joseph R. Biden Jr."))

# join on candidate names
endorsings = semi_join(polls, Endorsements, "candidate_name")

# count number of endorsements for each candidate
endorsings_grouped = endorsings %>%
  group_by(candidate_name) %>%
  summarise(count=n())

# plot candidates' number of endorsements
p = ggplot(data=endorsings)+
  geom_bar(mapping=aes(x=candidate_name))
p = p + theme_dark()
ggsave("dark_plot.png", p)
p_labelled = p + xlab("candidate_name") + ylab("number of endorsements") + theme_minimal()
ggsave("labelled_plot.png", p_labelled)


# question 4-GOOD
# read packages
library(tidyverse)
library(tm)
library(lubridate)
library(wordcloud)
# read dataset
setwd("C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/problem_sets/ps3")
tweets = read.csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')

# split date and time
tweets = tweets %>%
  mutate(created_at = str_split(created_at, " ")) %>%
  mutate(date = unlist(str_split(tweets$created_at, " "))[(1:nrow(tweets)%%2 != 0)]) %>%
  mutate(time = unlist(str_split(tweets$created_at, " "))[(1:nrow(tweets)%%2 == 0)])
# date range
min_date = min(tweets$date, na.rm = TRUE)
max_date = max(tweets$date, na.rm = TRUE)
# earliest date: "2014-01-01"
# latest date: "2020-02-14"

# subset into retweets
tweets = tweets %>%
  filter(!is_retweet)
# 5 most popular tweets
tweets_pop = tweets %>%
  arrange(desc(favorite_count))
most_pop_tweets = tweets_pop[1:5, "text"]
# 5 most popular:
# [1] "A$AP Rocky released from prison and on his way home to the United States from Sweden. It was a Rocky Week get home ASAP A$AP!"                                                                                                                                                             
# [2] "https://t.co/VXeKiVzpTf"                                                                                                                                                                                                                                                                   
# [3] "All is well! Missiles launched from Iran at two military bases located in Iraq. Assessment of casualties &amp; damages taking place now. So far so good! We have the most powerful and well equipped military anywhere in the world by far! I will be making a statement tomorrow morning."
# [4] "MERRY CHRISTMAS!"                                                                                                                                                                                                                                                                          
# [5] "Kobe Bryant despite being one of the truly great basketball players of all time was just getting started in life. He loved his family so much and had such strong passion for the future. The loss of his beautiful daughter Gianna makes this moment even more devastating...."
# 5 most retweeted tweets
tweets_retweet = tweets %>%
  arrange(desc(retweet_count))
most_retweeted_tweets = tweets_retweet[1:5, "text"]
# 5 most retweeted:
# [1] "#FraudNewsCNN #FNN https://t.co/WYUnHjjUjg"                                                                                                                                       
# [2] "TODAY WE MAKE AMERICA GREAT AGAIN!"                                                                                                                                               
# [3] "Why would Kim Jong-un insult me by calling me \"old\" when I would NEVER call him \"short and fat?\" Oh well I try so hard to be his friend - and maybe someday that will happen!"
# [4] "A$AP Rocky released from prison and on his way home to the United States from Sweden. It was a Rocky Week get home ASAP A$AP!"                                                    
# [5] "Such a beautiful and important evening! The forgotten man and woman will never be forgotten again. We will all come together as never before"


# cleaning tweets
tweets$text = gsub("\\s+"," ",tweets$text)
tweets$text = gsub("\\d", "", tweets$text)
tweets$text = gsub('[[:punct:]]+','', tweets$text)
tweets$text = tolower(tweets$text)
stopwords = c("see","people","new","want","one","even","must","need","done","back","just","going", "know",
              "can","said","like","many","like","realdonaldtrump")
stopwords_std = stopwords(kind = "SMART")
stopwords = c(stopwords, stopwords_std)
# tweets$text = tweets$text[!(tweets$text %in% stopwords)]
toRemove <- paste0("\\b(", paste0(stopwords, collapse="|"), ")\\b")
tweets$text = gsub(toRemove,'', tweets$text)


# creating data
library(tidytext)
tweets_words <-  tweets %>%
  select(text) %>%
  unnest_tokens(word, text)
words <- tweets_words %>% count(word, sort=TRUE)


# creating word cloud
set.seed(1234) # for reproducibility 
wc = wordcloud(words = words$word, freq = words$n, min.freq = 3,
               max.words=200, random.order=FALSE, 
               rot.per=0.35,
               colors=brewer.pal(8, "Dark2"))

# creating document term matrix
text = tweets$text
docs = Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs, control =
                            list(weighting = weightTfIdf))

matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
top_words = df$word[1:50]
# top 50 words
# [1] "trump"                 "great"                 "president"            
# [4] "amp"                   "america"               "run"                  
# [7] "donald"                "make"                  "country"              
# [10] "vote"                  "time"                  "love"                 
# [13] "big"                   "makeamericagreatagain" "good"                 
# [16] "foxandfriends"         "today"                 "true"                 
# [19] "democrats"             "hillary"               "news"                 
# [22] "tonight"               "foxnews"               "obama"                
# [25] "job"                   "dont"                  """                    
# [28] "win"                   "enjoy"                 "show"                 
# [31] "american"              "day"                   "jobs"                 
# [34] "years"                 "man"                   "nice"                 
# [37] "work"                  "border"                "media"                
# [40] "watch"                 "amazing"               "cnn"                  
# [43] "fake"                  "happy"                 "bad"                  
# [46] "deal"                  "world"                 "poll"                 
# [49] "clinton"               "night"                
> 
  #citations
  # https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mine-colorado-flood-tweets-science-r/
  # https://stackoverflow.com/questions/13854279/gsub-a-list-of-words-in-a-paragraph
  # https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a