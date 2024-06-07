
###TAYLOR DAMANN###
#####PDS PS3#####
#18/18
########ggplot#########
#1- finish what we started in class 2/11/20 -GOOD
rm(list = ls())
library(ggplot2)
##Here, I am loading in data, only choosing Super Tuesday states and relevant candidates. 
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer"),]

#I make a preliminary plot that shows the total for candidates across all of these states.
#Notice that Bloomberg's percent is in the negative and has a huge margin of error. This is
#because there isn't data for Bloomberg. R has to interpolate. 
ggplot(data=primaryPolls)+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name))
#We can interpret this plot by noting that Bernie Sanders and Joe Biden are the top-polling candidates,
#with Bernie on an upswing and Biden on a decline. Yet, this plot is too general. We can separate
#by state. The data separated into the 12 Super Tuesday states are below:
ggplot(data=primaryPolls)+
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.9) + 
  facet_wrap(~ state, nrow=2)
#From this plot, we can see who is popular in which states. In Maine, Minnesota,
#Vermont, and California Elizabeth Warren leads the polls. Biden takes the lead
#in Alabama, North Carolina, Tennessee, Texas and Virginia. Bernie has the lead
#in only Colorado and Utah. Unfortunately, we cannot do smoothing on some of
#these states due to lack of data. 

#We can make our plots more appealing by taking a few steps. First, I use the
#minimal theme by using theme_minimal(). Next, I add my own labels by using labs().
#I also fix the jumble of dates by using theme(axis.text.x). Finally, I add a title
#to the plots.

#by state
ggplot(data=primaryPolls) +
  ggtitle("Candidate Support by State") +
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.9) +
  labs(x="Polling Date", y="Percentage of Support", color = "Candidates")+
  facet_wrap(~ state, nrow=2)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=90, hjust=1)) 

#by candidate -GOOD
ggplot(data=primaryPolls)+
  ggtitle("Support for Candidates") +
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name)) + 
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.4) +
  facet_wrap(~ candidate_name, nrow=2)+
  theme_minimal() +
  labs(x="Polling Date", y="Percentage of Support", color = "Candidates") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=90, hjust=1)) 

#2- finish what we started on 2/13/2020
library(dplyr)
library(tidyr)

#only relevant candidates
relevant <- primaryPolls %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer")) %>%
  select(candidate_name, pct, state)
dim(relevant) #567x3

#reorganizing
relevant <- relevant %>%
  pivot_wider(names_from = state, values_from = pct)
dim(relevant)

#comparing sizes
object.size(primaryPolls)
object.size(relevant)


######tidyverse######
#3 -GOOD

#reading in data
library(fivethirtyeight)
library(tidyverse)
library(readr)
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020

#changing endorsements variable name endorsee to candidate_name
Endorsements <- rename(Endorsements, candidate_name = endorsee)
colnames(Endorsements)

#changing endorsements data frame into a tibble object
Endorsements <- as_tibble(Endorsements)
class(Endorsements)

#filtering poll to only have the 6 Klobuchar, Sanders, Warren, Biden, Bloomberg, Buttigieg
#and to only candidate_name, sample_size, start_date, party and pct
subpolls <- polls %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")) %>%
  select(candidate_name, sample_size, start_date, party, pct)
subpolls

#find different spellings in the datasets and make them the same
#with only DPLYR
#we can see through setdiff that in subpolls, the names are Bernard Sanders and Joseph R. Biden Jr. 
#we can change these to Bernie Sanders and Joe Biden
setdiff(subpolls$candidate_name, Endorsements$candidate_name)
subpolls <- subpolls %>%
  mutate(candidate_name = recode(candidate_name, "Bernard Sanders" = "Bernie Sanders", "Joseph R. Biden Jr." = "Joe Biden"))

#checking that I have five candidates
intersect(subpolls$candidate_name, Endorsements$candidate_name)

#combining the two datasets by candidate name using dplyr
combined <- subpolls %>%
  inner_join(Endorsements, by = "candidate_name")
unique(combined$candidate_name)

colnames(combined)
#creating a variable which indicates number of endorsements for
#each of the five candidates
countEndorse <- combined %>%
  group_by(candidate_name) %>%
  add_tally() %>%
  filter(!is.na(candidate_name))
colnames(countEndorse)

#plot the number of endorsements each of the 5 candidates have
#save as p
p <- ggplot(data=countEndorse, mapping=aes(x=candidate_name))+
  geom_bar()

#run p + theme_dark()
p + theme_dark()

#change x and y labels, adding title, picking a theme
p <- ggplot(data=countEndorse, mapping=aes(x=candidate_name, fill=candidate_name))+
  geom_bar()

p + theme_classic() +
  ggtitle("Endorsements of Democratic Candidates for POTUS")+
  labs(x="Democratic Candidates", y="Number of Endorsements") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=0, hjust=0.5)) 

###text as data###
#4 -GOOD
library(tidyverse)
library(tm)
library(lubridate)
library(wordcloud)
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')

#separating the created_at variable
as_tibble(tweets)
tweets <- tweets %>% 
  separate(created_at, c("Date", "Time"), sep = " ")  

#report ranges of dates in dataset
dateRange <- range(tweets$Date)
dateRange

#removing retweets
tweets <- tweets %>%
  filter(is_retweet == FALSE)

#finding most favorited tweets
tweets <- tweets %>%
  arrange(desc(favorite_count))
tweets$text[1:5]

#finding most retweeted tweets
tweets <- tweets %>%
  arrange(desc(retweet_count))
tweets$text[1:5]

#Remove extraneous whitespace and making all lowercase
words <- str_split(tweets$text, pattern = " ")
words <- str_c(unlist(words))
words <- str_to_lower(words)

#removing punctuation and special characters
words <- str_replace_all(words, "[^[:alnum:]]", "")

#removing all numbers
words <- removeNumbers(words)

#removing stop words and blank spaces
fillers <- c(stopwords("en"), "see", "im", "people", "new", "want", "one", "even", "must", "need", "done", "back", "just", "going", "know", "can", "said", "like", "realdonaldtrump", "will", "rt", "")
content <- words[!(words %in% fillers)]
length(content)

#making a word cloud requires frequencies of terms
frequencies <- as.tibble(content) %>%
  group_by(word = value) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#making the actual word cloud
wordcloud(frequencies$word, frequencies$count, min.freq = 3, max.words = 50)

#Create a document term matrix called DTM that includes the argument 
#control = list(weighting = weightTfIdf)
content <- VectorSource(tweets$text)
contentCorpus <- SimpleCorpus(content, control = list(language = "en"))
DTM <- DocumentTermMatrix(contentCorpus, control = list(removeNumbers = T, stopWords = T, weighting = weightTfIdf))
dim(DTM)


#reporting 50 words with highest tf.idf scores using a lower frequency bound of .8
head(findFreqTerms(DTM, lowfreq = 0.8), n=50)