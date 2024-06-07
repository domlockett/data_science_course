### 10.5/12
###TAYLOR J. DAMANN###
#### PDS PS2 #####


#1
#In a for loop, we put in an argument that tells it how long to loop. I set this
#to loop from 1 to 7. Following the curly brackets, a for loop needs to know what
#to do, so I instruct it to print the cube of each number that it iterates over. 
for(i in 1:7){
  print(i^3)
}

#2
### -1.5
### You never resampled the dice casts, so you misunderstood what was supposed to occur if the value of the first roll was >7. Also, I wanted the sum of the roll to equal 2 or 6, not one of the die. Since this was somewhat amiguous, I only subract a half point for that part. 

#I set the seed at 14 first so that my answers are the same as the rest of my
#classmates' answers. Then, I ask the for loop to iterate from 1 to 1000 because
#the question asks for 1000 simulations. Each iteration will be a single simulation.
#Telling the for loop what to do, I ask it to sample two numbers with replacement
#from 1 to 6, the numbers on the faces of dice. Now I add some conditional statements.
#If the two numbers sum to between 8 and 12 inclusively, the game ends. I make an if
#statement to do this, asking for the sum of each specific iteration. If the sum is
#between 8 and 12, the function breaks and prints out the values. If not, the iterations
#continue until a 2 or a 6 is rolled.
set.seed(14)

rolls <- NULL
for(j in 1:1000){
  for(i in 1:50){
    x <- sample(1:6, 2, replace=T)
    a.roll <- sum(x)
    if(a.roll >= 8 & i==1) {
      rolls <- c(rolls,i)
      break
    } else if (i != 1 & x[1] == 2 | x[2] == 2 | x[1] == 6 | x[2] == 6) {
      rolls <- c(rolls,i)
      break
    }
  }
}

#Below is the average number of rolls produced each game. 
mean(rolls)

#3
###Good

#First, I load in the dataset using the readr package. This package is useful for letting R figure out
#how to deal with what data you are reading in. I call this GSS_data and use the View function
#to pull up the data in a new window. 
library(readr)
GSS_data <- read_csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
View(GSS_data)

##Using plyr, I can see the actual counts for each president. I do this just so I can see whether
#I end up being correct. 
#library(plyr)
#count(GSS_data, 'pres16')

#Now I create my function using a series of if statements. Using the which function allows me to
#grab only the elements equal to the president name I am looking for. Using length around which
#give me an actual count.
vote.choice <- function(candidate) {
  if(candidate == "Clinton") {
    countClinton <- length(which(GSS_data$pres16 == "Clinton"))
    print(countClinton)
  } else if(candidate == "Trump") {
    countTrump <- length(which(GSS_data$pres16 == "Trump"))
    print(countTrump)
  } else if(candidate == "Other") {
    countOther <- length(GSS_data$pres16) - (577+764)
    print(countOther)
  } else
    print("Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response.")
}
#Now I print my results to show that everything worked. 
vote.choice("Trump")
vote.choice("Clinton")
vote.choice("Other")

#4
### Good
#The first step is to install and load in the library. Using the View function
#opens a new window with this data so I can view it. 

#install.packages("fivethirtyeight")
library(fivethirtyeight)
View(cabinet_turnover)

#Now I start to build my function. This function takes in the argument "president"
#followed by several if statements. I am using the logical "or" command here because
#several of the presidents served the same number of days. 
appoint <- function(president) {
  appointServe <- mean(cabinet_turnover$days[cabinet_turnover$president == president], na.rm=T)
  if(president == "Carter" | president == "Bush 41") {
    return(appointServe/1461)
  } else if(president == "Reagan" | president == "Clinton") {
    return(appointServe/2922)
  } else if(president == "Bush 43" | president == "Obama") {
    return(appointServe/2922)
  } else if(president == "Trump") {
    return(appointServe/1105)
  } else {
    return("Please enter Carter, Reagan, Bush 41, Clinton, Bush 43, Obama or Trump.")
  }
}

#Now I check to see if my answer matches the one that Jacob offered, and it does!
appoint("Reagan")


#5
###Good
#First I want to open a tab with the data so I can view them. 
View(congress_age)

#below I am just seeing if these lines of code work...
#mean(congress_age$age[congress_age$congress == 113])

#congress_df <- NULL
#for(i in 80:113) {
#means <- mean(congress_age$age[congress_age$congress == i])
#congress_df <- rbind(congress_df, data.frame(i, means))
#print(congress_df)
#}

#state_df <- NULL
#for(i in unique(congress_age$state)) {
#means <- mean(congress_age$age[congress_age$state == i])
#state_df <- rbind(state_df, data.frame(i, means))
#print(state_df)
#}

#Above, you can see the product of me trying to run commands
#before attempting to create a function. Now I put these into my
#function. The user should specify whether they want to see
#'congress' or 'state', which they will put as an argument.
#The argument the user inputs will route them through if statements
#to one of the three outputs below. Each output is created using
#a simple for loop. 

congress_stats <- function(x) {
  if(x == "congress") {
    congress_df <- NULL
    for(i in 80:113) {
      means <- mean(congress_age$age[congress_age$congress == i])
      congress_df <- rbind(congress_df, data.frame(i, means))
      print(congress_df)
    }
  } else if(x == "state") {
    state_df <- NULL
    for(i in unique(congress_age$state)) {
      means <- mean(congress_age$age[congress_age$state == i])
      state_df <- rbind(state_df, data.frame(i, means))
      print(state_df)
    }
  } else print("Please input either 'congress' or 'state'.")
}

#Testing if it works, and it does!
congress_stats("congress")
congress_stats("state")
congress_stats("impeachment")

