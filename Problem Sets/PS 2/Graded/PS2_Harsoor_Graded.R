---
  title: "4625 PS2"
author: "Diva Harsoor"
date: "February 12, 2020"
output: html_document
---
###Grade: 11/12
### -1 for only 2 commits
#1) prints out cubes of 1 through 7
#Good
  for (val in 1:7) {
    print(val^3)
  }

#2) 1000 simulations of a game 
# that ends if the first roll is 8,9,10,11,12
# and if not ends when the dice sum to 2 or 6
# prints the average number of rolls in each game over 1000 simulations
# Good

set.seed(14)
num_rolls = 0
for (i in 1:1000) {
  if (sum(sample.int(6, size = 2, replace = TRUE)) > 7) {
    num_rolls = num_rolls + 1
  } else {
    current.sum = 0
    while (current.sum != 2 && current.sum != 6) {
      num_rolls = num_rolls + 1
      current.sum = sum(sample.int(6, size = 2, replace = TRUE))
    }
  }
}
avg_rolls = num_rolls/1000
print(avg_rolls) #4.052

#3)
###Good

#doing this outside the function so we don't have to do it every time
gss <- read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
gss.df <- as.data.frame(gss)

# function vote.choice takes "Trump", "Clinton", and
# "Other" as a parameter and returns the corresponding 
# number of votes from the 2016 election
vote.choice <- function(candidate) {
  if (candidate != "Trump" && candidate != "Clinton" && candidate != "Other") {
    return("Please enter either 'Trump' 'Clinton' or 'Other' into the function
to return a valid response")
  } else if (candidate == "Other") {
    return(nrow(gss.df) - nrow(subset(gss.df, pres16 == "Trump")) - nrow(subset(gss.df, pres16 == "Clinton")))
  }  else {
    return(nrow(subset(gss.df, pres16 == candidate)))
  }
}

vote.choice('Trump')
vote.choice('Clinton')
vote.choice('Other')
vote.choice('Othesfr')

#4)
###Good

# some infrastructure for the appoint function
install.packages('fivethirtyeight')
library(fivethirtyeight)
turnover.df <- data.frame(cabinet_turnover)
names <- c("Carter", "Reagan", "Bush 41", "Clinton", "Bush 43", "Obama", "Trump")
days <- c(1461, 2922, 1461, 2922, 2922, 2922, 1105)
presidents.df = data.frame(names, days)
subset(presidents.df, names == "Carter")["days"][1,1]

# function appoint takes "Carter", "Reagan", "Bush 41", "Clinton",
# "Bush 43", and "Obama" as a parameter and returns the avg number 
# of days appointees served during the corresponding administration
appoint <- function(pres) {
  current <- na.omit(subset(turnover.df, president == pres)["days"])
  avg <- sum(current)/nrow(current)
  days <- subset(presidents.df, names == pres)["days"][1,1]
  return (avg/days)
}

#5
###Good
age.df <- data.frame(congress_age)

# function congress_stats takes "congress" and "state" as a parameter
# for congress, it returns the avg age of the 80th to 113th congresses
# and for state, it returns the avg age of congress members for each of
# the 50 states
congress_stats <- function(category) {
  if (category != "congress" && category != "state") {
    print("This function can only take \"congress\" and \"state\" as its argument Please try again.")
    return -1
  }
  if (category == "congress") {
    ages = c()
    index = c()
    for (i in 80:113) {
      ages = c(ages, mean(na.omit(subset(age.df, congress == i))[["age"]]))
      index = c(index, i)
    }
  }
  else if (category == "state") {
    ages = c()
    index = c()
    for (code in state.abb) {
      ages = c(ages, mean(na.omit(subset(age.df, state == code)[["age"]])))
      index = c(index, code)
    }
  }
  return (data.frame(ages, index))
}

congress_stats('state')
congress_stats('congress')
