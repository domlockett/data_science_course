
library(tidyverse)

#load and look at data
candidatelevel1 <- read_csv('CandidateLevel.csv')
dim(candidatelevel1)
# [1] 830  14

pollingdata1 <- read_csv('PollingData1992-2016.csv')
dim(pollingdata1)
# [1] 13486    18

#View(candidatelevel1)
#View(pollingdata1)
colnames(candidatelevel1)
colnames(pollingdata1)

#Make a list of races with more than 2 candidates
remove <- candidatelevel1$RaceID[candidatelevel1$RaceID %in% names(which(table(candidatelevel1$RaceID) >2)) ]
length(remove)
#[1] 35

#Remove the races with more than 2 candidates and remove all louisana races from polling data.
sum(pollingdata1$state == 'Louisiana')
#[1] 118

pollingdata2 <- pollingdata1[! pollingdata1$RaceID %in% remove & ! pollingdata1$state == 'Louisiana',]
dim(pollingdata2)
# [1] 12696    18

#tack on incumbent status and weighted and percentage raised experience to candidate data

PollingData <- merge(x = pollingdata2, y = candidatelevel1[ , c('Candidateidentifier',"Incumbent", "weightexperience", 'PercentageRaised')], by = "Candidateidentifier", all.x=TRUE)


#Create a binary indicator of whether the candidate won
PollingData$win <- ifelse(PollingData$Percentage.of.Vote.won.x > 50, 1, 0)

dim(PollingData)
# [1] 12696    22

write.csv(PollingData, 'PollingCandidateData92-16.csv')

remove(list = ls())

#load and look at data
candidatelevel1 <- read_csv('CandidateLevel2018.csv')
dim(candidatelevel1)
# [1] 73 15


pollingdata1 <- read_csv('PollingData2018.csv')
dim(pollingdata1)
#[1] 586  18

#make name consistent
candidatelevel1$RaceID <- candidatelevel1$raceid
#View(candidatelevel1)
#View(pollingdata1)
colnames(candidatelevel1)
colnames(pollingdata1)

#Make a list of races with more than 2 candidates
remove <- candidatelevel1$RaceID[candidatelevel1$RaceID %in% names(which(table(candidatelevel1$RaceID) >2)) ]
length(remove)
# [1] 9

#Remove the races with more than 2 candidates (here there were no cases for louisana and lots of NA's for states 174)
pollingdata2 <- pollingdata1[! pollingdata1$RaceID %in% remove ,]

#those nine case appeared 18 times
dim(pollingdata2)
# [1] 568  18


#tack on incumbent status and weighted and percentage raised experience to candidate data; names are slightly

PollingData <- merge(x = pollingdata2, y = candidatelevel1[ , c('Candidateidentifier',"Incumbent", "Weightedexperience", 'Percentage Raised')], by = "Candidateidentifier", all.x=TRUE)

#Create a binary indicator of whether the candidate won
PollingData$win <- ifelse(PollingData$Percentage.of.Vote.won.x > 50, 1, 0)

dim(PollingData)
#[1] 568  22


write.csv(PollingData, 'PollingCandidateData18.csv')
