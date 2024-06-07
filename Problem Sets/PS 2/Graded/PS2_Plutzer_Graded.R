###Grade: 12/12

rm(list = ls())

#Q1
###Good
for (n in seq(7)) {
  print(n^3)
}

#Q2
###Good
set.seed(14)

rolls_results = rep(0,1000) #Initialize the results array
for (iteration in seq(1000)) {
  dice1 = sample(seq(6),1)
  dice2 = sample(seq(6),1)
  rolls = 1 # Count the initial roll
  sum = dice1 + dice2
  if (sum < 8) {
    while ((sum != 2) & (sum != 6)) {
      rolls = rolls + 1
      dice1 = sample(seq(6),1)
      dice2 = sample(seq(6),1)
      sum = dice1 + dice2
    }
  }
  rolls_results[iteration] = rolls
}
print(mean(rolls_results)) #Returns 3.433


#Q3
###Good

data3 = read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv",stringsAsFactors = F)

vote.choice = function(candidate) {
  #
  #Provides the number of people who voted for a given candidate.
  #Inputs: candidate - (string) Candidate name; 'Trump', 'Clinton', or 'Other'
  #Outputs: NULL if invalid candidate name
  #         (numeric) Number of rows in data3 where candidate was given as answer for pres16 column
  #
  valid_candidates = c("Trump","Clinton","Other")
  if (!(candidate %in% valid_candidates)) {
    print("Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response.")
    NULL
  }
  else if (candidate == "Other"){
    return(nrow(data3[(data3$pres16 != "Trump") & (data3$pres16 != "Clinton"),]))
  }
  else {
    return(nrow(data3[data3$pres16 == candidate,]))
  }
}

vote.choice("Trump")
vote.choice("Clinton")


#Q4
###-1/4
### Good but we didn't get the same values. 

library('fivethirtyeight')

appoint = function(president) {
  #
  #Provides the proportion of time cabinet appointees spent under a given president
  #Inputs: president - (string) President name since Carter
  #Outputs: NULL if invalid president name (prints a list of valid presidents)
  #         (numeric) Proportion of time averaged accross appointees 
  #
  valid_presidents = c("Carter","Reagan","Bush 41","Clinton","Bush 43","Obama","Trump")
  pres_terms = c(1461,2922,1461,2922,2922,2922,1105)
  if (!(president %in% valid_presidents)) {
    print("Please enter a valid President into the function to return a valid response.")
    print("Valid Presidents:")
    print(valid_presidents)
    NULL
  }
  else {
    return(mean(cabinet_turnover[cabinet_turnover$president == president,]$length, na.rm = T)/
             pres_terms[which(valid_presidents == president)])
  }
}
appoint('Reagan')

#Q5
###Good
library('fivethirtyeight')

congress_stats = function(statstype) {
  #
  #Provides information about the average of congressmembers by congress or state
  #Input: statstype - (string) Type of groupings for the average data; by "congress" or "state"
  #Outputs: (data.frame) Dataframe of the ages with the rownames corresponding to the group (either the congress number or state)
  #         NULL if neither "congress" nor "state" was provided as input
  #
  if (statstype == "congress") {
    congresses = unique(congress_age$congress)
    avg_age = rep(0,length(congresses)) #Initializing my age column
    for (c in seq(length(congresses))) {
      avg_age[c] = mean(congress_age[congress_age$congress == congresses[c],]$age)
    }
    ages = data.frame(avg_age)
    rownames(ages) = congresses
    return(ages)
  }
  else if (statstype == "state") {
    states = unique(congress_age$state)
    avg_age = rep(0,length(states)) #Initializing my age column
    for (s in seq(length(states))) {
      avg_age[s] = mean(congress_age[congress_age$state == states[s],]$age)
    }
    ages = data.frame(avg_age)
    rownames(ages) = states
    return(ages)
  }
  else {
    print("Please enter a valid argument into the function.")
    print("Valid Arguments: 'congress' or 'state'")
    NULL
  }
}

congress_stats('state')
congress_stats('congress')
