#ggplot2 exercises
# 18/18
#Problem 1 - 2/11 - COMPLETE -GOOD
library(ggplot2)
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama","Arkansas","California","Colorado","Maine","Massachusetts","Minnesota","North Carolina","Oklahoma","Tennessee","Texas","Utah","Vermont","Virginia"),]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg","Tom Steyer"),]
ggplot(data=primaryPolls)+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name)) + 
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.4) +
  facet_wrap(~state, nrow=4) +
  theme_minimal() + xlab("Date of Survey") + ylab("Percent of Vote")+guides(fill = guide_legend(title = "Candidate Name"))

#Problem 2 - 2/13 - COMPLETE -GOOD
library(dplyr)
library(tidyr)
library(readr)
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")
primaryPolls.subcand<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg","Tom Steyer"),]
candidate.state.dyad <- primaryPolls.subcand %>%
  group_by(candidate_name, state) %>%
  summarise(average_candidate=mean(pct), count=n())
object.size(primaryPolls)
object.size(primaryPolls.subcand)
object.size(candidate.state.dyad)
#20512 in slimmed dataset compared to the original 4333880 bytes and the subsetted 1575128 bytes

#tidyverse exercises

#Problem 3 - COMPLETE -GOOD
library(fivethirtyeight)
library(tidyverse)
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020
Endorsements <- rename(Endorsements, candidate_name=endorsee) #rename endorsee variable to candidate_name
as_tibble(Endorsements) #change Endorsements data.frame to tibble
polls <- polls %>%
  filter(candidate_name%in%c("Amy Klobuchar","Bernard Sanders","Elizabeth Warren","Joseph R. Biden Jr.","Michael Bloomberg","Pete Buttigieg")) %>%
  select(candidate_name, sample_size, start_date, party, pct)
Endorsements %>%
  group_by(candidate_name) %>%
  summarise(count = n())
#Based on this, we see that "Bernard Sanders" is referred to as "Bernie Sanders" and "Joseph R. Biden Jr." is referred to as "Joe Biden"
#Thus, we change their names in Endorsements to the ones reflected in polls
Endorsements$candidate_name<-recode(Endorsements$candidate_name, 'Bernie Sanders'='Bernard Sanders')
Endorsements$candidate_name<-recode(Endorsements$candidate_name, 'Joe Biden'='Joseph R. Biden Jr.')
polls.endorsements <- polls %>%
  inner_join(Endorsements,by="candidate_name")
candidates<-unique(polls.endorsements$candidate_name)
unique(polls.endorsements$candidate_name) #shows only 5 candidates
#But, this dataset is clearly wrong because for each candidate's endorser appears for every poll
#Given this flaw in the joined data set, I will calculate the number of endorsements from the Endorsements dataset, not the joined one
polls <-Endorsements %>% group_by(endorser, candidate_name) %>% mutate(count = n())
Endorsements <- Endorsements %>%
  group_by(candidate_name, endorser) %>%
  mutate(endorsements.count=n())
plot.table <- polls %>%
  filter(candidate_name%in%candidates) %>%
  group_by(candidate_name) %>%
  summarise(count=n())
p<- ggplot(data=plot.table)+
  geom_point(mapping=aes(x=candidate_name,y=count,size=.8))+
  scale_y_continuous(name="# of Endorsements")
p+theme_dark()
p+xlab("Candidate Name")+labs(title="Endorsements of Candidates")+theme_classic()


#Text as Data exercises

#Problem 4 - COMPLETE
library(tidyverse) 
library(tm)
library(lubridate) 
library(wordcloud)
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')
tweets <- separate(tweets,created_at,into=c("date","time"),sep=" ",remove=TRUE)
range(tweets$date) #shows that 1/1/2014 and 9/9/2019 are the first and last dates reported
tweets <- tweets %>%
  filter(str_detect(text,"^RT \\@")==FALSE)
favorites.index <- match(tail(sort(tweets$favorite_count),5),tweets$favorite_count)
retweets.index <- match(tail(sort(tweets$retweet_count),5),tweets$retweet_count)
tweets[favorites.index,] #returns rows with the top 5 highest favorite counts
tweets[retweets.index,] #returns rows with the top 5 highest retweet counts
tweets$text<- str_remove_all(tweets$text,"[0-9]")
tweets$text<- str_remove_all(tweets$text,"[:punct:]")
tweets$text<- str_remove_all(tweets$text,"  ")
tweets$text<- tolower(tweets$text)
stopwords <- stopwords(kind="en")
stopwords <- str_c(" ",stopwords," ")
stopwords <- c(stopwords," see "," people "," new "," want "," one "," even "," must "," need "," done "," back "," just "," going "," know "," can "," said "," many "," like "," realdonaldtrump ")
stopwords <- str_c(stopwords,collapse="|")
tweets$text<- str_remove_all(tweets$text,stopwords)
trumpwords <- unlist(str_split(tweets$text,pattern=" "))
#trumpwords <- sort(table(trumpwords), decreasing=T)
trumpwords50 <- head(sort(table(trumpwords), decreasing=T),50)
wordcloud(names(trumpwords50),trumpwords50)
trumpcorpus <- Corpus(VectorSource(names(trumpwords50)))
DocumentTermMatrix(trumpcorpus,control = list(weighting = weightTfIdf))
#tf idf
#source: https://iyzico.engineering/how-to-calculate-tf-idf-term-frequency-inverse-document-frequency-from-the-beatles-biography-in-c4c3cd968296
#only calculated up to 50 words because doing more kept causing my R to freeze
tf <- trumpwords50 / sum(trumpwords50)
idf <- c()
for (i in names(trumpwords50)) {
  count <- 0
  for (j in tweets$text) {
    if(str_detect(j,i)==TRUE) {
      count <- count + 1
    }
  }
  idf <- c(idf, count)
}
idf <- length(tweets$text) / idf
idf <- log(idf)
tfidf <- tf * idf
sort(tfidf,decreasing=TRUE) #reports words in decreasing order in terms of tf-idf