#Isaac Lee
#438148
#Problem Set 2
#Due February 13, 10:00 AM
###Grade: 10.5/12

#1. 
numbers <- c(1:7) 
for(i in numbers){print(i^3)}

#2. 
### -1.5
### Two problems. I wanted the SUM of the two dice to equal 2 or 6, not a single die in a row, since this was somewhat ambiguous you only loose half a point. For every simulation you should have saved the number of times you had to roll the die until the simulation ended, then you should have found the average dice cast for each simulation
set.seed(14)
for(i in 1:1000) {
  twodice <- sample(1:6,2,replace=T)
  print(twodice)
  sumdice <- sum(twodice)
  if (i==1 & sumdice>=8 & sumdice <=12) {
    print ("avg number of dice cast per game: 1") 
    break
  } else if (i!=1 & (twodice[1]==2 | twodice[2]==2 | twodice[1]==6 | twodice[2]==6)){
    print(paste("avg number of dice cast per game: ",i))
    break }
}

#3.
###Good

#download gss dataset
gss <- read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
#create vote.choice function
vote.choice <- function(x) {
  if(x=="Trump") {
    return(length(gss$pres16[gss$pres16=="Trump"]))
  }
  else if (x=="Clinton") {
    return(length(gss$pres16[gss$pres16=="Clinton"]))
  }
  else if (x=="Other"){
    return(length(gss$pres16[gss$pres16!="Trump" & gss$pres16!="Clinton"]))
  }
  else { #Function edited so that if invalid word is entered, function returns message.
    return(print("Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response."))
  }
}

vote.choice("Clinton") #Check to see function works
vote.choice("Trump")
vote.choice("Dump Trump") #Check to see invalid response works

#4.
###Good


#Run the following code
install.packages('fivethirtyeight')
library(fivethirtyeight)
#Review Cabinet_turnover
head(cabinet_turnover)
summary(cabinet_turnover)
#Create appoint function
appoint <- function(pres){
  databypresident <- cabinet_turnover[cabinet_turnover$president == pres, ] 
  #subset the data by president
  databypresidentmean <- as.numeric(mean(databypresident$days, na.rm = TRUE)) 
  #make variables for number of days served by presidents
  days1 <- 1461
  days2 <- 2922
  days3 <- 1105
  #find the avg days served by appointees, by president
  if (pres == "Bush 41" | pres == "Carter"){
    return(databypresidentmean/days1)
  } else if (pres == "Obama" | pres == "Clinton" | pres == "Bush 43" | pres == "Reagan") {
    return(databypresidentmean/days2)
  } else if (pres == "Trump") {
    return(databypresidentmean/days3)
  } 
}
#Check that the value for Reagan matches what is given in the problem set
appoint("Reagan") #Value of .73 matches value given in problem set
#Try other values as well
appoint("Carter") #.86 
appoint("Trump") #.45

#5. 
###Good
#make an object with the necessary data 
ca<-congress_age
#create the function
congress_stats <- function(x) {
  if(x=="congress") {
    era <- unique(ca$congress) #get all unique congressional era values
    congressages<-c() #create an empty vector for congressages
    for(i in era) { #for loop for average age in each era
      avgage<-round(mean(congress_age$age[ca$congress==i]), digits =1) #calculate avg age to 1 digit
      congressages<-c(congressages,avgage)
    }
    agedatabyera<-data.frame("Avg Age"=congressages, "Congress Era"=era)
    return(agedatabyera)
  }
  else if (x=="state") { #repeat the same general procedure as above for age by state.
    state <- unique (ca$state)
    stateages <- c()
    for (i in state) {
      avgage2<-round(mean(congress_age$age[congress_age$state==i]), digits=1)
      stateages<-c(stateages,avgage2)
    }
    agedatabystate<-data.frame("Avg Age"=stateages, "State"=state)
    return(agedatabystate)
  }
}

#check that the function works properly
congress_stats ("congress")
congress_stats ("state")