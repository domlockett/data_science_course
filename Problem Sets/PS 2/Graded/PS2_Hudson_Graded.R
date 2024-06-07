## Grade: 12/12
# part 1
###Good 
for(i in seq(1,7)){
  print(i^3)
}


# part 2
###Good
max.rolls<-1000
game.roll.count<-0 # keeps track of how many times the dice have been rolled in the current game
games.completed<-0 # keeps track of how many games have been completed
total.rolls<-0 # keeps track of how total rolls done in all of the completed games
# (don't want to divide max.rolls by games completed because we might have not completed a game on the 1000th roll)

#set.seed(14)
for(i in seq(1,max.rolls)){
  roll<-sample(1:6, 2, replace=TRUE) # simulate rolling two dice
  #print(paste('roll:', roll))
  game.roll.count<-game.roll.count+1 # increase roll count of current game by 1
  if(game.roll.count==1 & sum(roll) %in% seq(8,12)){
    # if it's the first roll of the game and you get between 8 and 12 then end the game
    games.completed<-games.completed+1 # increase the number of finished games
    total.rolls<-i # set total rolls to correspond to number of finished games
    #print(paste('finished on first round.', 'total rolls:', total.rolls, 'games.completed:', games.completed))
    game.roll.count<-0 # current rolls for the current game is reset
  }
  else if(game.roll.count!=1 & sum(roll) %in% c(2,6)){
    # roll sums to 2 or 6 and it's not the first roll
    games.completed<-games.completed+1
    #print(paste('finished on round:', i-total.rolls,'total rolls:', i, 'games.completed:', games.completed))
    total.rolls<-i
    game.roll.count<-0
    
  }
}


avg.rolls <- total.rolls / games.completed
print(paste('average number of rolls is:', avg.rolls, 'therefore average number of dice is:', avg.rolls))


# part 3
###Good 

webData<-url("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
df <- read.csv(webData)


# description: returns the number of participants who voted for a candidate
# input: string of "Clinton", "Trump", or "Other"
# output: numeric of participants who voted for the input. String if input is not valid
vote.choice<-function(candidate){
  if(candidate %in% c('Clinton', 'Trump', 'Other')){
    # if input is valid
    if(candidate == 'Other'){
      # if the input is other then set the candidate string to match the value in the data set
      candidate = 'Other candidate (specify)'
    }
    return(sum(df[,'pres16'] == candidate)) # get number of participants whos vote matches the candidate
  }
  else{
    # input is not valid
    return("Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response")
  }
  
}
 vote.choice('Clinton')
 vote.choice('Trump')
 vote.choice('Other')
 vote.choice('a')


# part 4
#Good BUT I had to add a return() function to confirm the function ran
 
install.packages('fivethirtyeight')
library(fivethirtyeight)
head(cabinet_turnover)




# description: gets the number of days a president served in office
# input: president's name as a string
# output: number of days the president served in office as a numeric
get.term.time<-function(president){
  # create dictionary in the form of a dataframe to get serving time of each president
  presidents<-unique(cabinet_turnover$president)
  time<-c(1461,2922,1461,2922,2922,2922,1105)
  term.time<-data.frame('president'=presidents, 'time'=time) # create the dataframe
  return(term.time[term.time$president==president,'time']) # the the row that has the president and the column that is the time that they have been in office
}


# description: gets the proportion of time appointees spent serving each adminstration
# input: string of either "Carter", "Reagan", "Bush 41", "Clinton", "Bush 43", "Obama", or "Trump"
# output: numeric of the number of days appointees served for each administration, on average, divided by the number of days the particular president served
appoint<-function(president.name){
  mask<-cabinet_turnover$president==president.name # create mask to filter out all entries that aren't of the president passed in the argument
  data<-cabinet_turnover[mask,] # apply mask
  data<-data[!is.na(data$days),] # remove entries that have have no length
  
  avg.days<-mean(data$days) # get average time for oppointee
  proportion<-avg.days/get.term.time(president.name) # divide it by number of days the president served in office
}

appoint('Reagan')
# a



# part 5
### Good
### I get NA's when I run the function. I've tried many ways to resolve this to no avail. 

# description: groups congress data by input variable and finds the average age of each group
# input: string of either: "congress" or "state"
# output: average age of each group given by the input variable
congress_stats<-function(group.var){
  
  groups<-unique(congress_age[,'state']) # get all levels of the group variable
  df<-data.frame('group'=groups, 'avg.age'=NA) # create data frame to store data
  
  for(group in groups){
    # for each level in the group
    group_mask<-congress_age[,'state'] == group # mask to only get entries in the level
    data<-congress_age['state',] # apply the mask
    avg.age<-mean(data$age)
    df[df$group==group,'avg.age']<-avg.age # get entry in the data frame to store data that corresponds to the level and set the average age to the computed value
  }
  return(df)
  
}
congress_stats('state')
congress_stats('congress')