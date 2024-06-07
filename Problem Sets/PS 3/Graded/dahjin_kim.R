##### Political Data Science
##### Problem Set 3
##### Jin Kim
# 18/18
#start out clean
rm(list=ls())

##### 1- GOOD
#calling packages
library(ggplot2)
library(readr)

#retrieving data
primaryPolls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date <- as.Date(primaryPolls$start_date, "%m/%d/%Y")

#subset by relevant states and candidates
March3States <- c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia")
primaryPolls1 <- primaryPolls[primaryPolls$state %in% March3States, ]
relevantCandidates <- c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer")
primaryPolls1 <- primaryPolls1[primaryPolls1$candidate_name%in% relevantCandidates, ]

#putting them all on the same map
graph0 <- ggplot(data=primaryPolls1)+
  scale_shape_manual(values=1:nlevels(primaryPolls1$candidate_name))+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name))
graph0
#overall, Warren and Biden's popularity is going down. Sanders is rising.
#Bloomberg is also rising, but his confidence interval is very large due to missing data.
#Buttigieg seems more steady, and Klobuchar and Steyer is slightly rising.

#plotting by candidates in minimal theme, different axis, and customized legend name
graph1 <- ggplot(data=primaryPolls1)+
  scale_shape_manual(values=1:nlevels(primaryPolls1$candidate_name))+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name)) + 
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.4) +
  facet_wrap(~ candidate_name, nrow=2) +
  theme_minimal() +
  labs(x = "Dates", y = "Poll Percentage") + 
  ylim(-10, 50) + 
  scale_colour_discrete(name="Relevant\nCandidates")
graph1
#this allows us to see where the observations for each candidate lie.
#we can note the difference of poll frequencies among candidates.



##### 2 -GOOD
rm(list = ls())
#reload data as tibble
library(tidyverse)
primaryPolls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date <- as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls2 <- primaryPolls

#subset the data set for relevant candidates with state polls
sub.pP2 <- primaryPolls2 %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer"))

#new dataset with one row for each candidate-state dyad
pivot.pP2 <- sub.pP2 %>%
  pivot_wider(names_from = start_date, values_from = pct)

#compare size
object.size(sub.pP2)
object.size(pivot.pP2)
object.size(sub.pP2) < object.size(pivot.pP2)
#the new dataset has a much larger data size



##### 3- GOOD
rm(list = ls())
library(fivethirtyeight)
library(tidyverse)
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020

#Change the Endorsements variable name endorsee to candidate_name
Endorsements <- rename(Endorsements, candidate_name = endorsee)

#Change the Endorsements dataframe into a tibble object
Endorsements <- as_tibble(Endorsements)

#subset polls data accordingly
polls3 <- polls %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")) %>%
  select(candidate_name, sample_size, start_date, party, pct)

#compare candidate names and make them the same
# 1) check unique names in both dataset
unique(polls3$candidate_name)
unique(Endorsements$candidate_name)
#Bernie and Biden are different
# 2) make these the same
Endorsements <- Endorsements %>%
  mutate(candidate_name = recode(candidate_name,  "Joe Biden" = "Joseph R. Biden Jr.")) %>%
  mutate(candidate_name = recode(candidate_name,  "Bernie Sanders" = "Bernard Sanders"))

#combine the two dataset by candidate name
fulldata <- polls3 %>%
  inner_join(Endorsements, by = "candidate_name")
#check to see we have 5 candidates
unique(fulldata$candidate_name)

#create a variable for number of endorsement
endorsement.count <- fulldata %>%
  group_by(candidate_name) %>%
  summarise(count = n())

#plot endorsement count
p <- ggplot(data=endorsement.count) +
  geom_col(mapping = aes(x=candidate_name, y=count, fill=candidate_name))
p

#diff options & save the plot
p + theme_dark()
endorse.plot <- p + theme_dark()

#more playing around
newplot <- p + 
  theme_classic() +
  labs(x="Candidate", y="number of endorsement") +
  ggtitle("Number of Endorsements by Candidates") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
newplot



##### 4-GOOD
#preparation
rm(list=ls())
library(tidyverse)
#install.packages('tm')
library(tm)
#install.packages('lubridate')
library(lubridate)
#install.packages('wordcloud')
library(wordcloud)
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')

#separate the created_at variable into two new variables
tweets4 <- tweets %>%
  separate(created_at, c("date", "time"), sep = " ")
tweets4$date <- as.Date(tweets4$date, "%m/%d/%Y")

#report the range of dates that is in this dataset.
summary(tweets4$date)
#the oldest tweet is 2014-01-01, the newest tweet is 2020-02-14

#remove retweets
tweets4.og <- tweets4 %>%
  filter(is_retweet==FALSE)

#find the most retweeted/favorited tweet
top5fav <- tweets4.og %>%
  top_n(5, favorite_count) %>%
  arrange(desc(favorite_count))
top5fav$text
top5rt <- tweets4.og %>%
  top_n(5, retweet_count) %>%
  arrange(desc(retweet_count))
top5rt$text

#clean the tweets
clean.tweets <- tweets4.og %>%
  select(text) %>%
  mutate(text = tolower(text)) %>%
  mutate(text = removeNumbers(text)) %>%
  mutate(text = removePunctuation(text)) %>%
  mutate(text = str_replace_all(text, "[^[:alnum:]]", " ")) %>%
  mutate(text = removeWords(text, stopwords("en"))) %>%
  mutate(text = removeWords(text, c("see", "people","new","want","one","even","must","need", "done","back","just","going", "know", "can", "said","like","many","like","realdonaldtrump"))) %>%
  mutate(text = removeWords(text, "amp")) %>%
  mutate(text = stripWhitespace(text))

#create wordcloud
wordcloud(words = clean.tweets$text, max.words = 50, min.freq = 3, random.order = F)

#document term matrix
clean.tweets <- Corpus(VectorSource(clean.tweets$text))
Dtm <- DocumentTermMatrix(clean.tweets, control = list(weighting = weightTfIdf))

#report 50 with highest tf.idf, low frequency bound .8
top50Dtm <- findFreqTerms(Dtm, lowfreq = 0.8)[1:50]
top50Dtm