###Grade: 11.5/12

#Alex Newman Problem Set 2

#Problem 1.

#Iterate from 1 to 7 and print the cube of each number
for (i in 1:7) {
  print(i^3)
}

#problem 2
###-1/2
### Good but I was looking for the sum of the dice equaling 2 or 6. since i only had to add sum() to your function only missing half a point. good job

#set the seed
set.seed(14)
#initiate casts, the vector that will contain each game's number of dice casts
casts<-NULL
#simulate two dice being rolled in game 1000 times

for(iterator in 1:1000){
  roll<-sample(x=1:6, size = 2, replace=TRUE)
  rollnum<-1
  #end game immediately if dice sum totals to certain values
  if(sum(roll)%in% c(8,9,10,11,12)){
    casts<-c(casts, rollnum)
    next
  }
  #else roll dice until either a 2 or 6 is rolled
  repeat{
    roll<-sum(sample(x=1:6, size = 2, replace=TRUE))
    rollnum<-rollnum+1
    if(any(roll==2|roll==6)){
      casts<-c(casts, rollnum)
      break
    }
  }
}

#find average number of dice casts per game
mean(casts)

#problem 3
### Good

#read the dataset in
df<-read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
#create function vote.choice
vote.choice<- function(candidate){
  #return trump vote count
  if(candidate=="Trump"){
    return(length(which(df$pres16=="Trump")))
  }
  #return clinton vote count
  if(candidate=="Clinton"){
    return(length(which(df$pres16=="Clinton")))
  }
  #return other vote count
  if(candidate=="Other"){
    return(length(which(df$pres16=="Other candidate (specify)")))
  }
  #handle invalid inputs
  return("Please input either Trump, Clinton, or Other")
}

vote.choice('Clinton')
vote.choice('Trump')
vote.choice('other')
vote.choice('Other')
#Problem 4
###Good

library(fivethirtyeight)

turnover<-cabinet_turnover

#create function appoint that shows average number of days appointees served in administration given the input of the president
appoint<- function (president){
  #set the term length to calculate average
  if(president=="Carter"|president=="Bush 41") {
    termlength<-1461
  }
  
  if(president=="Reagan"|president=="Clinton"|president=="Bush 43"|president=="Obama"){
    termlength<-2922 
  } 
  
  if(president=="Trump"){ 
    termlength<-1105
  }
  
  
  #calculate average number of days in the administration
  avgdays<-mean(turnover$days[turnover$president==president],na.rm=TRUE)
  #return the average proportion of time appointees spent serving administration
  return(avgdays/termlength)
}

appoint('Trump')
appoint('Clinton')

#Problem 5
#Good


#function provides age information by either congressional era or choice given a choice between "congress" or "state"
#returns a list of years/states and the average age for each
congress_stats<-function(choice){
  #if congress is selected, goes through each unique era and calculates average age in that era
  if(choice=="congress"){
    eralist<-unique(congress_age$congress)
    agelist<-NULL
    for(era in eralist){
      era.age<-mean(congress_age$age[congress_age$congress==era])
      agelist<-c(agelist, era.age)
    }
    return(cbind(eralist, agelist))
  }
  #if state selected, goes through each unique state and calculates average age in that state
  if(choice=="state"){
    statelist<-unique(congress_age$state)
    agelist<-NULL
    for(state in statelist){
      state.age<-mean(congress_age$age[congress_age$state==state])
      agelist<-c(agelist, state.age)
    }
    return(cbind(statelist, agelist))
  }
  
}

congress_stats('state')
congress_stats('congress')
