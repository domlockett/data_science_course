data1 <-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
library(tidyverse)
library(tm)
library(fivethirtyeight)
data2 <- endorsements_2020
colnames(data2)[colnames(data2) == 'endorsee'] <- 'candidate_name'


primaryPolls <- as_tibble(data1)
endorsements01 <- as_tibble(data2)

primaryPolls <- filter(primaryPolls, candidate_name %in% c("Amy Klobuchar","Bernard Sanders","Elizabeth Warren","Joseph R. Biden Jr.","Michael Bloomberg","Pete Buttigieg"))


primaryPolls2 <- primaryPolls %>%
  select(candidate_name, sample_size, start_date, party, pct)
  
primaryPolls2$candidate_name <-  recode(primaryPolls2$candidate_name, "Joseph R. Biden Jr." = "Joe Biden", "Bernard Sanders" = "Bernie Sanders")




primaryPolls2<-primaryPolls2 %>%
  inner_join(endorsements01, by= 'candidate_name')


polls <- primaryPolls2 %>% group_by(endorser, candidate_name) %>% mutate(count = n())




p <-  ggplot(data = polls)+
  geom_point(mapping =aes(
    x = candidate_name, 
    y = count)) + 
  theme_bw()
 

endorsers <- filter(polls, endorser %in% rownames(table(polls$endorser)[1:10])) 



data3 <- read.csv('commencement_speeches.csv', stringsAsFactors = F)
plotThat(Data=gss, Y=gss$attend7 , X= senate_seat_forecast$win_probability, color=senate_seat_forecast$incumbent)


tweets <- read_csv('trump_tweets.csv')

tweets$created_at <- mdy_hm(tweets$created_at)
tweets <- separate(data = tweets, col = created_at, into  = c('Date', 'Time'), sep = ' ')
tweets <- tweets %>%
  filter(is_retweet==F)

tweets$text[match(sort(tweets$retweet_count, decreasing=TRUE)[1:5],tweets$retweet_count)]
library(tm) 

tweets <- gsub('$','/$',tweets$text)
Corpus <- with(tweets, VCorpus(VectorSource(text))) 


Corpus <- Corpus %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(PlainTextDocument)%>%
  tm_map(removeWords, c(stopwords("english"),"see", "people",'new','want','one', 'even','must','need','done','back','just','going', 'know', 'can','said','like','many','like','realdonaldtrump'))
strwrap(as.character(Corpus[[1]]))


library(wordcloud) 
wordcloud(Corpus, max.words = 50, colors = topo.colors(n = 50), random.color = TRUE, min.freq =3)

DTM <- DocumentTermMatrix(Corpus, control = list(weighting = weightTfIdf))


findFreqTerms(DTM)[1:50]


          