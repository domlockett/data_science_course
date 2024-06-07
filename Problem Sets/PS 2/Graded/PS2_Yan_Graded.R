### Grade: 9/12
### -2 points for only 1 commit

#1
###Good
seq=c(1:7) #store numbers 1 to 7 in a vector
for(val in seq){ #iterate over all the elements in seq
  print(val^3) #print the cube of each element in each iteration
}

#2
###Good 

# I find the instrustion of this question extremely unclear
set.seed(14) #ensure the number generated are the same
totalDiceCast=0 #variable which keep track of total dice cast in the sitmulation

for(i in seq(1:1000)){ #iterate for 1000 times
  #first roll
  die1=sample(1:6,1)  #one of the two dice being rolled
  die2=sample(1:6,1)  #one of the two dice being rolled
  total=die1+die2     #sum of the two dice
  i2=0 #declare new variable to keep tracm of the number of dice casted in one iteration
  i2=i2+2 #increment by 2 since every since there are two dice being thrown in one roll
  print(total) #test statement
  if(total!=8||total!=9||total!=10||total!=11||total!=12){ #Check if the first roll does not equal one of those five values
    while(die1!=2&&die1!=6&&die2!=2&&die2!=6){ #continue to roll the dice until you roll either a 2 or a 6
      die1=sample(1:6,1)  #one of the two dice being rolled
      die2=sample(1:6,1)  #one of the two dice being rolled
      i2=i2+2 #increment by 2 since every since there are two dice being thrown in one roll
    }
  }
  totalDiceCast=i2+totalDiceCast #increment the total dice cast in the end of each iteration
}

avgDiceCast=totalDiceCast/1000 #avg the dice casts per iteration
print(avgDiceCast)

##3
#Good

data1<-read.csv('http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv', stringsAsFactors = F)
vote.choice<-function(x){
  count=0;
  
  if(x=="Trump"){
    for(i in data1$pres16){ #for loop iterate through the participants
      if(i=="Trump"){
        count=count+1 #increment the count
      }
    }
    return(count)
  }
  
  else if(x=="Clinton"){
    for(i in data1$pres16){ #for loop iterate through the participants
      if(i=="Clinton"){
        count=count+1 #increment the count
      }
    }
    return(count)
  }
  
  else if(x=="Other"){
    for(i in data1$pres16){ #for loop iterate through the participants
      #since the instruction did not specify, I assumed anyone who do not have "Clinton" or "Trump"
      #in column pres16 have not vote for neither of them, and that include those who answered "dont know"
      #"not applicable"
      if(i!="Trump"&&i!="Clinton"){
        count=count+1
      }
    }
    return(count) 
  }
  
  else{
    return("Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response")
  }
}

#test cases
vote.choice("Trump")
vote.choice("Clinton")
vote.choice("Other")
vote.choice("Others")

#4
#install.packages('fivethrityeight')
#library(fivethirtyeight)
#I was not able to install this package
#Warning in install.packages :package 'fivethrityeight' is not available (for R version 3.6.0)
#So I just looked it up, and found the csv file on github

### -1
### The function returns an error stating avgDays not found. I will give you a point despite this because length was the appropriate value, you were correct.

data2<-read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/cabinet-turnover/cabinet-turnover.csv', stringsAsFactors = F)
appoint<-function(x){
  ##if you serve from the begining of the administration to the end
  if(x=="Carter"||x=="Bush 41")
    full=1461
  else if(x=="Reagan"||x=="Clinton"||x=="Bush 43"||x=="Obama")
    full=2922
  else if(x=="Trump")
    full=1105
  
  count=0
  totalDays=0
  iterator=0 #iterator value to keep track of a second column
  
  for(i in data2$president){ #for loop iterate through the presidents
    iterator=iterator+1
    if(i==x){ # select appropriate case
      count=count+1
      if(is.na(data2$length[iterator])||grepl("combined",data2$length[iterator])||data2$length[iterator]==""){
        totalDays=totalDays+full #in situations that the administration is not over
      }
      else{
        totalDays=totalDays+as.numeric(data2$length[iterator])
      }
    }
  }
  
  #series of ifelse statements to differ the denominator of the division
  if(x=="Carter"||x=="Bush 41")
    return(avgDays/1461)
  else if(x=="Reagan"||x=="Clinton"||x=="Bush 43"||x=="Obama")
    return(avgDays/2922)
  else if(x=="Trump")
    return(avgDays/1105)
  else
    return("Please enter one the following value:'Carter','Reagan','Bush 41','Clinton','Bush 43','Obama','Trump'")
}

appoint('Obama')
appoint("Reagan") #I dont think your data on reagan is accurate, did you mistake days for length, because days shows how many days into the administration
#this result is biased due to my incapability of substracting dates formatted mm/dd/yy

#5
###Good
data3<-read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/congress-age/congress-terms.csv', stringsAsFactors = F)
congress_stats <- function(x){
  if(x=="congress"){
    for(i in c(80:113)){ #outer loop for #of congress
      iterator=0      #state trackers
      count=0
      totalAge=0
      for(k in data3$congress){ #for loop iterate through entire dataframe
        iterator=iterator+1
        if(k==i){ 
          totalAge=totalAge+as.numeric(data3$age[iterator]) #increment trackers
          count=count+1 
        }
      }
      print(c(totalAge/count,i)) #print as a vector, so they will be on the same line
    }
  }
  
  #same logic as above
  else if(x=="state"){
    for(i in state.abb){ #outer loop for #of congress
      iterator=0      #state trackers
      count=0
      totalAge=0
      for(k in data3$state){ #for loop iterate through entire dataframe
        iterator=iterator+1
        if(k==i){ 
          totalAge=totalAge+as.numeric(data3$age[iterator]) #increment trackers
          count=count+1 
        }
      }
      print(c(totalAge/count,i)) #print as a vector, so they will be on the same line
    }
  }
  
  else{
    return("please enter 'state' or 'congress' as argument")
  }
  
}

#test cases
congress_stats("congress")
congress_stats("state")
congress_stats("")
