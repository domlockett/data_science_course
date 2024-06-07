# Diva Harsoor
# February 27, 2019
# Political Data Science - Problem Set 3
# 18/18
library(ggplot2)
library(tidyverse)
library(fivethirtyeight)
library(tm)
library(lubridate)
library(wordcloud)

#1) GOOD
primaryPolls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls <- as.data.frame(primaryPolls)
primaryPolls$start_date <- as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPollsCopy <- primaryPolls # so we don't have to read in the data set again every time we change

# Subsetting the data to include only the Super Tuesday states and the relevant candidates
primaryPolls <- primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
primaryPolls <- primaryPolls[primaryPolls$candidate_name%in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer"),]

# Plotting the results
# Including changing theme, as well as axis and legend labels
# Also, fixed up the y-axis and the made the date labels easier to read
ggplot(data = primaryPolls, mapping = aes(x=start_date, y=pct, color=candidate_name))+ 
  geom_smooth() + 
  facet_wrap(~candidate_name) + 
  theme_minimal() +
  labs(title= "In Preparation for Super Tuesday: the Current State of the Race") + 
  labs(x="Date", y="Percent Polling", color="2020 Presidential Candidates") +
  scale_y_continuous(limits=c(-10,100)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))




#2) Worked on this with Charlie Yan- GOOD

primaryPolls <- primaryPollsCopy # resetting to the original data
pivot1 <- primaryPolls %>% group_by(candidate_name, state) %>% filter(candidate_name) %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden, Jr.", "Michael Bloomberg", "Pete Buttigieg") %>%
  summarise(mean = mean(pct)) # subsetting
pivot2 <- pivot_wider(pivot1, name_from = candidate_name, values_from = mean)

object.size(pivot2) # calculating sizes
object.size(primaryPolls)
# 6672 bytes v 1011304


#3)- GOOD
# Read in libraries at the top
# Reading in data/creating new variables below
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020

# Cleaning up the Endorsements dataset
Endorsements <- rename(Endorsements, candidate_name = endorsee)
Endorsements <- as_tibble(Endorsements)

# Cleaning up the polls dataset
polls <- filter(polls, candidate_name == c("Amy Klobuchar", "Bernard Sanders","Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
polls <- select(polls, candidate_name, sample_size, start_date, party, pct)

# Bringing the two datasets together
unique(Endorsements$candidate_name)
toString(unique(polls$candidate_name)) # disturbed that I got a factor
# Looks like there are only two with different names, Biden and Sanders

polls <- polls %>% mutate(candidate_name = recode(candidate_name, "Bernard Sanders" = "Bernie Sanders"))
polls <- polls %>% mutate(candidate_name = recode(candidate_name, "Joseph R. Biden Jr." = "Joe Biden"))

dems.race.2020 <- polls %>% inner_join(Endorsements, by = "candidate_name")
unique(dems.race.2020$candidate_name) # There are 5!

# Counting and plotting endorsements
num_endorsements <- dems.race.2020 %>% group_by(candidate_name) %>% filter(!is.na(candidate_name)) %>% summarise(count = n()) 
p <- ggplot(data = polls, mapping = aes(x = candidate_name, y = count)) +
  geom_point()
p + labs(title = "Who the Who's Who Think Should Win in 2020", x = "Number of Endorsements", y = "2020 Presidential Candidate") +
  theme_light() + 
  theme(axis.text.y = element_text(angle = 15), axis.text.x = element_text(angle = 15))



#4) Read in libraries at the top -GOOD
tweets <- read.csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')
tweetsCopy <- tweets # So we can skip reading in again
tweets <- as.tibble(tweets)

# Splitting out date and time variables from created_at
tweets <- tweets %>% separate(created_at, c("date_published", "time_published"), sep = " ")

# Finding date range
tweets <- as.data.frame(tweets)
date.data <- tweets %>% separate(date_published, c("month", "day", "year"), sep = "/")
yearRange = range(as.numeric(date.data$year)) #2014-2020
startMonth = range(as.numeric(unlist(filter(date.data, year == "2014")$month))) #1-12
endMonth = range(as.numeric(unlist(filter(date.data, year == "2020")$month))) #1-2
startDay = range(as.numeric(unlist(filter(date.data, year == "2014", month == "1")$day))) #1-31
endDay = range(as.numeric(unlist(filter(date.data, year == "2020", month == "2")$day))) #1-14
# So the date range is 1/1/2014 - 2/14/2020

# Dropping retweets
tweets <- tweets %>% select(source, text, date_published, time_published, retweet_count, favorite_count)

# Finding top 5 most liked tweets
tweets <- tweets %>% arrange(desc(favorite_count)) 
most_liked <- tweets[1:5,] # It was a Rocky Week get home ASAP A$AP!

# Finding top 5 most retweeted tweets
tweets <- tweets %>% arrange(desc(retweet_count))
most_retweeted <- tweets[1:5,] # Hehe #3

# Removing everything extraneous
tweets <- tweetsCopy
tweets$text = as.character(tweets$text)


tweet_words <- tweets %>% select(text) %>% 
  mutate(text = stripWhitespace(text)) %>%
  mutate(text = removeNumbers(text)) %>%
  mutate(text = removePunctuation(text)) %>%
  mutate(text = tolower(text)) %>%
  mutate(text = removeWords(text, stopwords('en'))) %>%
  mutate(text = removeWords(text, c("see", "people","new","want","one","even","must","need","done","back","just","going", "know","can","said","like","many","like","realdonaldtrump", "rt"))) %>%
  
  # Getting frequencies
sorted <- as.tibble(unlist(str_split(tweet_words, " "))) %>%
  group_by(value) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
sorted <- as.data.frame(sorted)
sorted <- sorted[-c(1, 2, 6, 9, 29, 35, 48),] # really tried

# Making the wordcloud
wordcloud(words = sorted$value, freq = sorted$count, min.freq=3, max.words=50, colors=c("red", "blue", "navy"))

# Making DTM
input <- Corpus(VectorSource(sorted[,1]))
DTM <- TermDocumentMatrix(input, control=list(weighting = weightTfIdf))

# Find the top 50 terms
findFreqTerms(DTM, lowfreq = 0.8)[1:50]

# They are:
# "will"        "great"       "trump"       "amp"         "president"  
# "now"         "get"         "america"     "country"     "thank"      
# "democrats"   "make"        "donald"      "big"         "time"       
# "never"       "news"        "vote"        "good"        "much"       
# "run"         "today"       "american"    "fake"        "border"     
# "years"       "best"        "media"       "obama"       "hillary"    
# "love"        "house"       "job"         "united"      "nothing"    
# "day"         "really"      "last"        "first"       "way"        
# "states"      "ever"        "win"         "bad"         "think"      
# "impeachment" "made"        "deal"        "jobs"        "state" 