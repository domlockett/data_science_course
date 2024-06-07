rm(list=ls())
library(ggplot2)

### PART 1 -GOOD
#18/18
# get tibble
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"),]

# get data from dsired states
poll.data<-primaryPolls[primaryPolls$state %in% c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]


ggplot(data=poll.data, mapping=aes(x=start_date, y=pct))+
  geom_point(mapping=aes(color=state))+
  geom_smooth(se=F)+
  facet_wrap(~candidate_name, nrow=3)+
  theme_minimal()+
  labs(x="Date", y="Percentage of Vote", title="State of Primary Race", color="State")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) # rotate x labels


### PART 2- GOOD
library(tidyverse)
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")

# get useful columns of data
basicPolls<-select(primaryPolls, state, candidate_name, start_date, pct, sample_size)

# get relevant candidates
basicPolls <- basicPolls %>% filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
basicPolls <- basicPolls %>% arrange(state)

# create new column that indicates how many votes the candidate received at each poll. There may be many polls in one state
basicPolls <- basicPolls %>% mutate(votesReceivedAtPoll = round(sample_size * (pct/100)))

basicPolls

# group the data by state and candidate and sum up the total number of votes received in each group.
# data tells us how many votes each candidate got in the entire state
by_state_candidate <- basicPolls %>% group_by(state, candidate_name)
by_state_candidate <- by_state_candidate %>% summarise(votesForCandidateInState=sum(votesReceivedAtPoll))

# need to find total number of votes that were cast in each state
votes_by_state <- basicPolls %>% group_by(state) %>% summarise(totalVotesInTheState=sum(votesReceivedAtPoll))

# for each column store the total number of votes in the state in order to find the percentage of the state that voted for each candidate
by_state_candidate <- by_state_candidate %>% inner_join(votes_by_state, totalVotesInTheState, by='state')
by_state_candidate <- by_state_candidate %>% mutate(percentageOfStateForCandidate = votesForCandidateInState/totalVotesInTheState)

# final dataset
by_state_candidate

print(paste('Original size:', object.size(primaryPolls), 'bytes. New size:', object.size(by_state_candidate), "bytes."))


### PART 3 -GOOD
library(fivethirtyeight)
library(tidyverse)
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020

Endorsements
Endorsements <- rename(Endorsements, candidate_name=endorsee)

# convert endorsements to tibble
Endorsements<-as_tibble(Endorsements)

# filter polls to only contain candidates with certain names. THen subset down to certain columns
polls$candidate_name
polls <- polls %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")) %>%
  select(candidate_name, sample_size, start_date, party, pct)


unique(Endorsements %>% select(candidate_name))
unique(polls %>% select(candidate_name))

# replace candidate_names that have "Bernard Sanders" with "Bernie Sanders", "Joseph R. Biden Jr." with "Joe Biden"
polls <- polls %>% mutate(candidate_name=replace(candidate_name, candidate_name=="Bernard Sanders", "Bernie Sanders"))
polls <- polls %>% mutate(candidate_name=replace(candidate_name, candidate_name=="Joseph R. Biden Jr.", "Joe Biden"))

polls
Endorsements

combined_data <- Endorsements %>% inner_join(polls, by='candidate_name')
unique(combined_data %>% select(candidate_name))


endorsements_by_candidate <- Endorsements %>% group_by(candidate_name) %>% summarise(number_of_endorsements = n()) %>% filter(!is.na(candidate_name))
endorsements_by_candidate
p<-ggplot(data=endorsements_by_candidate, mapping=aes(x=candidate_name, y=number_of_endorsements))+
  geom_point()
p+theme_dark()+theme(axis.text.x=element_text(angle=90, hjust=1)) # rotate x labels

p+theme_minimal()+labs(x='Candidate', y='Number of Endorsements', title='Endorsements by Candidate')+theme(axis.text.x=element_text(angle=90, hjust=1)) # rotate x labels


### PART 4 -GOOD
library(tidyverse)
#install.packages('tm')
library(tm)
#install.packages('lubridate')
library(lubridate)
#install.packages('wordcloud')
library(wordcloud)
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')


created_at_time<-unlist(str_split(tweets$created_at, pattern=" "))[c(F,T)]
created_at_time<-str_pad(created_at_time, 5, pad="0")

tweets <- tweets %>% mutate(created_at_date = as.Date(unlist(str_split(created_at, pattern=" "))[c(T,F)], "%m/%d/%Y"), created_at_time = created_at_time) 

# sort by date then by time within date. Can sort by date after using as.Date because dates are written as YYYY-MM-DD which will order by date if using lexicographical ordering
# in order to sort time using lexicographical ordering, we need to pad the hours with 0 so that the format is HH:MM, which we do above
# get the first and last elements of the tibble after they are sorted by date to get the range of dates
date_range<-(tweets %>% arrange(created_at_date, created_at_time) %>% select(created_at))[c(1,nrow(tweets)),]
date_range[1,]

print(paste('the tweets are from', date_range[1,], 'to', date_range[2,]))

original_tweets <- tweets %>% filter(is_retweet == FALSE)

# most popular tweets
most_liked <- (original_tweets %>% arrange(-favorite_count))[1:5,]
most_liked$text

# most retweeted tweets
most_retweeted <- (original_tweets %>% arrange(-retweet_count))[1:5,]
most_retweeted

# remove extra whitespace

# description: removes stop words from a list of string
# input: list of strings
# output: list of strings
get_non_stop<-function(tweet){
  list_of_stops<-c(stopwords('en'), c('see', 'people','new','want','one','even','must','need','done','back','just','going', 'know', 'can','said','like','many','like','realdonaldtrump'))
  a<-tweet[!(tweet %in% list_of_stops)]
  return(a)
}

processed_tweets<-original_tweets %>%
  select(text) %>%
  mutate(text=removePunctuation(text)) %>%
#  mutate(text=str_remove_all(text, "'|'|"|"|-|amp|RT")) %>%
  mutate(text=removeNumbers(text)) %>%
  mutate(text=tolower(text)) %>%
  mutate(text=removeWords(text, removePunctuation(c(stopwords('en'), c('see', 'people','new','want','one','even','must','need','done','back','just','going', 'know', 'can','said','like','many','like','realdonaldtrump'))))) %>%
  mutate(text=stripWhitespace(text)) %>%
  mutate(text=str_trim(text))

# use str_split to get all the words to be separated and ulist to get them all in one list
# then use a tibble to count up the number of occurences of each word
counts<-as_tibble(unlist(str_split(processed_tweets$text, pattern=" "))) %>%
  group_by(value) %>%
  summarise(count = n()) %>%
  arrange(-count)
counts

wordcloud(words=counts$value, freq=counts$count, min.freq=3, max.words=50)


# create DTM
corpus<-Corpus(VectorSource(processed_tweets$text))
matrixthing<-TermDocumentMatrix(corpus, control=list(weighting = weightTfIdf))

# get 50 words with highets tf-idf
library('slam')
sort(row_sums(matrixthing), decreasing=T)[1:50]