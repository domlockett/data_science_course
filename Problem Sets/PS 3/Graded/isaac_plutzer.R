rm(list = ls())
library(ggplot2)
library(dplyr)
# 15/18
#1- good
#load the dataset
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")

#Filter by states on super tuesday
superTuesday = c('Alabama','Arkansas','California','Colorado','Maine','Massachusetts','Minnesota','North Carolina','Oklahoma','Tennesee','Texas','Utah','Vermont')
primaryPolls = primaryPolls %>%
  filter(state %in% superTuesday)

#Filter by the top 6 frontrunner candidates as of Feb 26
frontrunners = c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")
primaryPolls = primaryPolls %>%
  filter(candidate_name %in% frontrunners)

#Lets add in the number of delegates associated with each state
state = superTuesday
delegates = c(52,31,415,67,24,91,75,110,37,64,228,29,16)
primaryPolls = left_join(primaryPolls,data.frame(state,delegates,stringsAsFactors = F),by = 'state')

ggplot(primaryPolls) +
  geom_point(mapping = aes(x = start_date,y = pct,color = delegates,),alpha = .8) +
  geom_smooth(mapping = aes(x = start_date,y = pct),se = F,color = 'violet',alpha = .6) + 
  facet_wrap(~ candidate_name,nrow = 2) +
  theme_minimal() +
  labs(title = "Super Tuesday Polls by Date for Democratic Race Frontrunners") + 
  labs() + 
  xlab("Start Date") + 
  ylab("Polling Percentage") + 
  labs(color = "State \nDelegates")

#2--GOOD
#Start again with the original data
data = read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
data$start_date<-as.Date(data$start_date, "%m/%d/%Y")

new_data = data %>%
  filter(candidate_name %in% frontrunners) %>%
  select(candidate_name,state,pct,start_date)
library(pryr)
object.size(new_data)
pivoted = pivot_wider(new_data,names_from = start_date,values_from = pct)
object.size(pivoted)
# This gives 211 rows for 6 candidates, meaning that not every possible combo of candidate-state actually shows up.-- CORRECT IF YOU LOOK AT THE NUMBER OF UNIQUE STATES, THERE ARE ONLY 38 SO A ROUGH APPX OF THE CORRECT NUMBER OF ROWS

#3-GOOD
library(fivethirtyeight)
polls = read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements = endorsements_2020
Endorsements = rename(Endorsements,candidate_name = endorsee)
Endorsements = as_tibble(Endorsements)
#Filtering and subsetting the polls data
polls = polls %>%
  filter(candidate_name %in% frontrunners) %>%
  select(candidate_name,sample_size,start_date,party,pct)
#Two discrepancies between the data...
polls$candidate_name = recode(polls$candidate_name, "Bernard Sanders" = "Bernie Sanders", "Joseph R. Biden Jr." = "Joe Biden")
#Doing an inner join of these two datasets (i have no idea why you want us to do this...)
joined_data = inner_join(Endorsements, polls, by = 'candidate_name')
#Counting the number of endorsements
num_endorsements = Endorsements %>%
  count(candidate_name) %>%
  filter(candidate_name %in% polls$candidate_name)
# Plotting
library(ggplot2)
p = ggplot(num_endorsements, aes(x = candidate_name,y = n)) + geom_bar(stat = "identity")
p = p + theme_dark()
p + ylab("Number of Endorsements") + xlab("Candidate")

#4 -3 YOU WERE ASKED TO SAVE THE WORDCLOUD TO YOUR REPOSITORY AND THE FREQUENCY SHOULD HAVE BEEN SET TO OCCURING 3 TIMES OR MORE; NEEDED TO MAKE A DOCUMENTTERMMATRIX NOT A TERMDOCUMENTMATRIX AND YOU WERE ASKED TO FIND THE FREQUENCY OF THE TOP 50 TERMS
rm(list = ls())
library(tidyverse)
library(tm)
library(lubridate)
library(wordcloud)
tweets = read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')
#Making date and time columns
tweets$date = as.Date(tweets$created_at,"%m,%d/%Y")
tweets$time = map_chr(tweets$created_at,function(x) {unlist(str_split(x,pattern = " "))[2]})
min(tweets$date)
max(tweets$date)
#Filter out retweets
tweets = tweets %>%
  filter(!is_retweet)
tweets = tweets %>%
  arrange(-retweet_count)
print(tweets[1:5,]$text)
extra_stop_words = c('see', 'people','new','want','one','even','must','need','done','back','just','going', 'know','can','said','like','many','like','realdonaldtrump')
text = tweets$text
text = paste(unlist(str_split(text,pattern = " ")),sep = " ")
text %>%
  gsub("https\\S*", "", text) %>%
  gsub("@\\S*", "", text) %>%
  gsub("amp", "", text) %>% 
  gsub("[\r\n]", "", text) %>%
  gsub("[[:punct:]]", "", text)
words = unlist(str_split(text,pattern = " "))
u_words = unique(words)
t_words = sort(table(words),decreasing = T)[1:50]

wordcloud(words = t_words,freq = 1)


textcorp = Corpus(VectorSource(text))
rm(text) #to save memory
toSpace = content_transformer(function (x , pattern ) gsub(pattern, " ", x))
textcorp = tm_map(textcorp, toSpace, "/")
textcorp = tm_map(textcorp, toSpace, "@")
textcorp = tm_map(textcorp, toSpace, "\\|")
textcorp = tm_map(textcorp, removeWords, extra_stop_words) 
textcorp = tm_map(textcorp, removePunctuation)
textcorp = tm_map(textcorp, stripWhitespace)
dtm = TermDocumentMatrix(textcorp,list(weighting = weightTfIdf))

