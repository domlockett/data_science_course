##### Danny Kim PS2 #####

### Grade: 8.5/12

rm(list=ls())

# Problem 1
###Good

for (i in 1:7){
  print(i^3)
}


# Problem 2
###-1
### You misunderstood a couple things. First, I want the average number of rolls for the 1000 simulations. So for every simulation, you should have saved the number of rolls you took. Because the problem was ambiguous, I only subtracted .5, but you also got the second part wrong. You should have stopped when the two dice had a sum of 2 or 6. 
set.seed(14)
for(i in 1:1000){
  dice <- sample(1:6,size=2,replace=TRUE)
  rollResult <- sum(dice)
  if(i ==1 & rollResult > 7){
    ## paste for the print
    print (paste("avg number of dice cast per game is", i))
    break
  } else if (i!=1 & (dice[1]==2 | dice[2]==2 | dice[1]==6 | dice[2]==6)){
    ## dice [#] shows the result of size 1's sample
    print (paste("avg number of dice cast per game is", i))
    break}
}

#problem3
### -1/2
### You didn't quite get the code right for other, try: else if(x!= "Clinton" & x!= "Trump" & x!= "Other")
p3ds <-read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
cleanP3<- p3ds$pres16
vote.choice <- function(x){
  if(x=="Trump"){
    #length <- to get a number of count
    trumpVotes <- length(cleanP3[cleanP3==x])
    return(trumpVotes)
  }
  if(x=="Clinton"){
    cliVotes <- length(cleanP3[cleanP3==x])
    return(cliVotes)
  }else if(x=="Other"){
    otherVotes <- length(cleanP3[v!="Trump" && cleanP3[cleanP3!="Clinton"]])
    return(otherVotes)
  }else{
    print("Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response")
  }
}
vote.choice("Clinton")
## 764
vote.choice("Trump")
## 577
vote.choice("Danny Kim")
## Error :(

# problem 4
###Good

## Install the library
install.packages('fivethirtyeight')
library(fivethirtyeight)

## call cabinent_turnover
## Create a dataframe to make it easier to calculate days


temp<-cabinet_turnover
tempP4 <- subset(temp)
appoint <- function(x){
  President <- c("Carter","Reagan","Bush 41","Clinton","Bush 43","Obama","Trump")
  term1 <- 1461
  term2 <- 2922
  termTrump <- 1105
  pplMean <- as.numeric(mean(tempP4$days[tempP4$president==x],na.rm=T))
  ## I added na.rm, because there is a missing value and if I dont put it, I cannot get my Reagan and Trump data to work
  if (x == "Bush 41" | x == "Carter"){
    return(pplMean/term1)
  } else if (x == "Obama" | x == "Clinton" | x == "Bush 43" | x == "Reagan") {
    return(pplMean/term2)
  } else if (x == "Trump") {
    return(pplMean/termTrump)
  } 
}

appoint("Reagan")
#.7327033
appoint("Carter")
# .8678


# problem 5
### -2 
###Neither of your functions worked, they returned the same number 50 times. try: 
congress_stats <- function(stats){
  if(stats == 'state'){
    for(i in 1:50){
      a<-  round(mean(congress_age$age[congress_age$state == state.abb[i]]), digits=1)
      print(paste(a,state.abb[i]), sep= " ")}
  }else if(stats =="congress"){
    for(i in 1:34){
      b<-round(mean(congress_age$age[congress_age$congress == unique(congress_age$congress)[i]]), digits =1)
      print(paste(b, unique(congress_age$congress)[i], sep= " "))}
  }}


tempP5 <- congress_age
congress <- congress_age$congress
age <- congress_age$age
state<- congress_age$state
## return the average age of congressmemebers for each congressional era
## congress -> 113 
congress_stats <- function(x) {
  if (x == "congress") {
    # For creating a list of congress 
    era <- unique(congress)
    avgAge <- c(NULL)
    for(i in era) {
      ## na.rm <- as mentioned in the previous problem
      mean1 <- mean(age[congress==i],na.rm= T)
      avgAgeEra <- c(avgAge, mean1)
    }
    return(data.frame(avgAgeEra, era))
  } else if (x == "state") {
    ## For creating a list of state 
    state <- unique(state)
    ## avgAgeS for avoiding a collision
    avgAgeS <- c(NULL)
    for(i in state) {
      mean2 <- mean(age[state==i],na.rm= T)
      avgAgeState <- c(avgAgeS, mean2)
    }
    return(data.frame(avgAgeState, state))
  } else{
    print("Type congress or state")  }
}

#test to see if it works
## Wasn't sure if we need to round it so I just left it
congress_stats("congress")
congress_stats("state")
congress_stats("Danny Kim")
