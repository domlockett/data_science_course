#(16/18) - Pending: Get me the code to problem 4 for full credit. You clearly did it.

#1 -GOOD
library(ggplot2)
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts","Minnesota", "North Carolina","Oklahoma", "Tennessee", "Texas","Utah", "Vermont","Virginia"),]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"),]

p<-ggplot(data=primaryPolls, fill = "Cadidates")+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name)) + 
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.4) +
  facet_wrap(~ state, ncol = 3,nrow=5)+
  theme_minimal() #changetheme
#graph by states

p +labs(name="Candidates", title="Plot of Polling Data by State (missing Vermount and Arkansas)",
        x ="dates", y = "support (%)")+ #change default axis names
  theme(legend.position="top",legend.title = element_text(colour="red", face="bold"),legend.background = element_rect(fill="lightblue",size=0.5, linetype="solid", colour ="darkblue"))
#change default legend


#2 Worked toghether with Diva - I gave wrong instruction here. GOOD
#install.packages("tidyr")
library(dplyr)
library(tidyr)
library(readr)
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')

piv1<-primaryPolls%>%group_by(candidate_name, state) %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")) %>% #limit this down to only the relevant candidates
  summarise(mean=mean(pct)) #new variable mean created to help present data

piv2<-pivot_wider(piv1, names_from = candidate_name, values_from = mean) #Re-organize the dataset so that there is only one row for each candidate-state dyad

#measure size
object.size(piv1) #6672 bytes
object.size(primaryPolls) #1011304 bytes

#3- GOOD
#install.packages("fivethirtyeight")
library(fivethirtyeight)
library(tidyverse)
#polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
#Error in open.connection(con, "rb") : HTTP error 400. So I did not use that line of code you provided in the document
polls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020
Endorsements<-rename(Endorsements, candidate_name=endorsee) #change candidate name using dplyr function
colnames(Endorsements)[6] <- "candidate_name" #change candidate name
as_tibble(Endorsements) #Change the Endorsements dataframe into a tibble object.
polls<-filter(polls, candidate_name == c("Amy Klobuchar", "Bernard Sanders","Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
#filter by names
polls<-select(polls,candidate_name, sample_size, start_date, party, pct)#subsetting the data

setdiff(Endorsements$candidate_name, polls$candidate_name) 
setdiff(polls$candidate_name, Endorsements$candidate_name) 

#output of the last two lines indicates that only "Joseph R. Biden Jr." "Bernard Sanders" are different across both tables
polls$candidate_name<-recode(polls$candidate_name,  "Bernard Sanders"="Bernie Sanders","Joseph R. Biden Jr."="Joe Biden")#rename bernie and biden
new<-polls %>% inner_join(Endorsements, by="candidate_name") #combine both data set into one dataset call "new"
new<-new %>% group_by(candidate_name) %>% summarise(numEndorsement=n())#Create a variable which indicates the number of endorsements for each of the five candidates using dplyr.
#Plot the number of endorsement each of the 5 candidates have. Save your plot as an object p
p<-ggplot(data=as_data_frame(new))+
  geom_point(mapping = aes(x=new$candidate_name, y=new$numEndorsement, color=candidate_name))

#run the following code: p + theme_dark(). Notice how you can still customize your plot without
#rerunning the plot with new options. 
p + theme_dark()

#Now, using the knowledge from the last step change the label of the X and Y axes to be more informative, add a title, and use your favorite theme. 
p+theme_minimal()+labs(title="Endorsement by Candidates",
                       x ="Names", y = "Number of Endorsement")
#4 Hmm. You don't have the code for this problem but you have the wordcloud so I know you did. -2 until you can get me the rest of the code

library(tidyverse) 
#('tm') 
library(tm) 
#install.packages('lubridate') 
library(lubridate) 
#install.packages('wordcloud') 
library(wordcloud)
tweets <- read.csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv', stringsAsFactors = F)