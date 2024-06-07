###Grade: 12/12

#Problem 1
for(i in 1:7) {
  print(i^3)
}
#Problem 2
### Good
### You got most of the logic right. 2 things. I wanted the average number of casts per simulation. The num.rolls object should be an object with 1000 observation. Within each observation you should have added 1 every time you rolled the dice like this: num.rolls[i] = num.rolls[i] + 1. The other problem is that I wanted the SUM of the dice rolls to equal 2 or 6. Since this was somewhat ambiguous, I only subtracted 1/2 a point here
set.seed(14)
num.rolls <- 0
for (i in 1:1000) {
  dice.rolls <- sample(1:6,2,replace=TRUE)
  sum.dice <- sum(dice.rolls)
  die.casts <- 1
  if(sum.dice>=8 & sum.dice<=12 & i == 1) {
    num.rolls <- c(num.rolls,die.casts)
    break
  }
  if(i!=1 & test.sum==FALSE) {
    repeat {
      dice.rolls2 <- sample(1:6,2,replace=TRUE)
      die.casts <- die.casts+1
      if(dice.rolls2[1]==2 | dice.rolls2[1]==6 | dice.rolls2[2]==2 | dice.rolls2[2]==6) {
        num.rolls<- c(num.rolls,die.casts)
        break
      }
    }
  }
}
print(mean(num.rolls))

#Problem 3
GSS<-read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
vote.choice = function(x) {
  if(x=="Trump") {
    length(GSS$pres16[GSS$pres16=="Trump"])
  }
  else if(x=="Clinton") {
    length(GSS$pres16[GSS$pres16=="Clinton"])
  }
  else if(x=="Other") {
    length(GSS$pres16[GSS$pres16!="Trump" & GSS$pres16!="Clinton"])
  }
  else {
    print("Please enter either 'Trump' 'Clinton' or 'Other' into the functionto return a valid response")
  }
}

#Problem 4
###Good

install.packages('fivethirtyeight')
library(fivethirtyeight)
cabinet_turnover <- subset(cabinet_turnover,is.na(cabinet_turnover$days)==FALSE)
preslist <- c("Carter","Reagan","Bush 41","Clinton","Bush 43","Obama","Trump")
presdayslist <- c(1461,2922,1461,2922,2922,2922,1105)
presdays<-data.frame("presidents"=preslist,"days"=presdayslist)
appoint <- function(pres) {
  avgday.appointee<-mean(cabinet_turnover$days[cabinet_turnover$president==pres])
  daysinoffice<-presdays$days[presdays$presidents==pres]
  return(avgday.appointee/daysinoffice)
}

appoint("Carter")
appoint("Reagan")
#Problem 5
###Good
congress_stats <- function(input) {
  if(input=="congress") {
    congress <- c(80:113)
    congressages<-c()
    for(i in congress) {
      avgage<-mean(congress_age$age[congress_age$congress==i])
      congressages<-c(congressages,avgage)
    }
    congress.ages.acc<-data.frame("Congress"=congress,"Average Age"=congressages)
    return(congress.ages.acc)
  }
  if(input=="state") {
    states <- unique(congress_age$state)
    congressages<-c()
    for(i in states) {
      avgage<-mean(congress_age$age[congress_age$state==i])
      congressages<-c(congressages,avgage)
    }
    state.ages.acc<-data.frame("State"=states,"Average Age"=congressages)
    return(state.ages.acc)
  }
}

congress_stats("state")
congress_stats('congress')
