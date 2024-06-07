#PS3 Gechun Lin
#17/18
library(ggplot2)
library(fivethirtyeight)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(tm)  
library(lubridate) 
library(wordcloud)
library(SnowballC)

#1 -GOOD
#download and subset data
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls%>%
  filter(state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"))
primaryPolls<-primaryPolls %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
#Because I'm not sure Jacob want a plot by state or by candidate, I made both:
# 1) plot by state
p1 <- ggplot(data=primaryPolls)+
  scale_shape_manual(values = 1:nlevels(primaryPolls$candidate_name))+
  geom_point(mapping = aes(x=start_date, y=pct,  color=candidate_name), alpha=.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~ state, nrow=5)
p1
#change to the minimial theme
p1 <- p1 + theme_minimal()
p1
#change the axis labels and legends
p1 + labs(x="Start Date", y="Percentage") + scale_colour_discrete(name="Candidates") 

# 2) plot by candiate
p2 <- ggplot(data=primaryPolls)+
  scale_shape_manual(values = 1:nlevels(primaryPolls$candidate_name))+
  geom_point(mapping = aes(x=start_date, y=pct,  color=candidate_name), alpha=.8)+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~ candidate_name, nrow=3)
p2
#change to the minimial theme
p2 <- p2 + theme_minimal()
p2
#change the axis labels and legends
p2 + labs(x="Start Date", y="Percentage") + scale_colour_discrete(name="Candidates") 


#2- GOOD
#re-organize the dataset so that there is only one row for each candidate-state dyad
primaryPolls<-read_.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
basic <- primaryPolls %>% 
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")) %>%
  select(candidate_name, state) %>%
  filter(state %in% state.name)
candidate.state <- summarise(group_by(basic, candidate_name, state), count=n()) %>%
  pivot_wider(names_from = state, values_from = count)

#compare the size of this dataset to our original dataset using the object_size command
(object.size(primaryPolls))
(object.size(candidate.state))

#3 -1 THESE PLOTS NEEDED TO BE SAVED TO YOUR REPOSITORY
polls <- read_csv(file="https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv")
Endorsements <- endorsements_2020
#change the Endorsements variable name endorsee to candidate_name
Endorsements <- rename(Endorsements, candidate_name=endorsee)
#change the Endorsements dataframe into a tibble object
as_tibble(Endorsements)
#filter the poll variable to only include the following 6 candidates
polls.6candidate <- filter(polls, candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
#subset the dataset to the following 5 variables
select(polls.6candidate, candidate_name, sample_size, start_date, party, pct)
#compare the candidate names
unique(Endorsements$candidate_name)
unique(polls.6candidate$candidate_name)
#make these names the same across datasets
Endorsements <- Endorsements %>%
  mutate(candidate_name=str_replace(Endorsements$candidate_name, "Bernie Sanders", "Bernard Sanders")) 
Endorsements <- Endorsements %>%
  mutate(candidate_name=str_replace(Endorsements$candidate_name, "Joe Biden", "Joseph R. Biden Jr."))
#combine the two datasets by candidate name
combine <- Endorsements %>%
  inner_join(polls.6candidate, by="candidate_name")
#create a variable which indicates the number of endorsements for each of the five candidates
combine.counts <- combine %>% count(candidate_name)
#plot the number of endorsement each of the 5 candidates have
p <- ggplot(data=combine, mapping=aes(x=candidate_name))+stat_count()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
p + theme_dark()
#using the knowledge from the last step change the label of the X and Y axes to be more informative, add a title, and use your favorite theme
p + labs(x="Candidates", y="Number of Endorsements", title="Each Candidate's Endorsements") + theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#4- GOOD
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')
#separate the created_at variable into two new variables where the date and the time are in separate columns
tweets <- tweets %>%
  separate(created_at, c("date", "time"), " ")
#report the range of dates that is in this dataset
range(tweets$date)
#subset the data to only include original tweets
original.tweets <- filter(tweets, is_retweet=="FALSE")
#show the text of the President's top 5 most popular and most retweeted tweets
popular <- original.tweets %>% arrange(desc(favorite_count))
retweeted <- original.tweets %>% arrange(desc(retweet_count))
popular[1:5, ]$text
retweeted[1:5, ]$text

#remove punctuation and number
text<- VCorpus(VectorSource(original.tweets$text))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
#convert to lower case
text <-tm_map(text, content_transformer(tolower))
#remove the standard english stop words
text <- tm_map(text,removeWords, stopwords("english"))
#remove the following words
text <- tm_map(text, removeWords, c("see", "people","new","want","one",
                                    "even","must","need", "done","back",
                                    "just","going", "know", "can", "said",
                                    "like","many","like","realdonaldtrump"))
#remove extraneous whitespace
text <- tm_map(text, stripWhitespace)
text <- tm_map(text, stemDocument)
wordcloud(text, max.words = 50, min.freq = 3,random.order=FALSE,
          colors=brewer.pal(8, "Accent"))

#create a document term matrix called DTM
DTM <- DocumentTermMatrix(text, control = list(weighting = weightTfIdf))

#report the 50 words with the the highest tf.idf scores using a lower frequency bound of .8
#this code only returns words without their tf.idf scores and it cannot order these words according to their tf.idf scores
findFreqTerms(DTM, lowfreq = 0.8)[1:50]
#my computer get stuck when running it...
DTM %>% as.matrix() %>%
  apply(MARGIN = 2, sum) %>%
  sort(decreasing = TRUE) %>%
  head(50)
