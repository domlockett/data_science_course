rm(list=ls())
#GRADE: 15.5/15
#Q1  
####GOOD

data <- read.csv("Expends2002.csv")
##### I opened it as read.table, because it is still a dataframe. => [1] "data.frame"
data.class(data)
colnames(data)

# Q2
#GOOD

colnames(data)[3] <-"Useless"
colnames(data)


# Q3
####GOOD


data$Useless <- NULL
data$Source <- NULL
colnames(data)
   

# Q4
####GOOD

data$EntType <- factor(data$EntType)
### changed to factor
levels(data$EntType)
##### The Variable "EntType" has 8 levels. 

# Q5 
####GOOD


### To Identify non-existent codes, I first listed all state values 
stateVar <- unique(c(as.character(data$State)))
stateVar
### Now I called state.txt, which had 50 state codes, I did this, because I don't know 
### all state codes.
state<- read.table("state.txt", header=TRUE)
USVar <- unique(c(as.character(state$column)))
USVar
### now I filtered, by comparing these two variables
filtered <- stateVar[!(stateVar %in% USVar)]
filtered
### I filtered these variables. "DC" "  " "GU" "VI" "AS" "St" "ZZ" "LL"
### I considered DC & GU & VI & AS as State Variable, because it is
### part of the terrotory. So I have "St" "ZZ" and "LL" that are non-existent codes
### next step is to recode these observations.

## 1)
data[data$State == "St",]
data[8994,]
data$State[data$State =="St"] <- "DC" 
##### change to DC based on looking at the row 
### 2)
data[data$State == "ZZ",]
data[12333,]
data$State[data$State =="ZZ"] <- "VI" 
##### change to VI based on looking at the row 
### 3)
data[data$State == "LL",]
data[12983,]
data$State[data$State =="LL"] <- "IA" 
##### changed to Iowa based on looking at the row 

# Q6
#####GOOD

### I first identified non-existent state code
data[data$State == "  ",]
data <- data[!(data$State %in% "  "),] 
##### I got 19912 observations after removing missing variable

# Q7
####GOOD

### first change data$Zip to numeric. They changes to four digit numbers
numZip <- as.numeric(data$Zip)
mean(numZip)
##### Answer : 2937.884 * This is after Q6 where I deleted a lot

# Q8- 1 : Descrip - Description
#####GOOD

### strsplit - splitting space between words
### apply -> gives output of length of each column
spl <- sapply(strsplit(as.character(data$Descrip), " "), length)
median(spl)
##### median = 2

# 8-2 numeric portion of CRPFilerid
#### - 1/2
# You got close. so you should have stopped on the 4th line and instead done: length(unique(c(as.character(final_end$V2))))==2243
#Partial credit because you were almost there


### first, change to character and split it to numeric and non-numeric (regex)
hi <-strsplit(as.character(data$CRPFilerid), split="[A-Z]+")
## this is to change the row and column to get rid of deleted non-numeric characters
final_end <- as.data.frame(t(data.frame(hi)))
final_end$V1 <-NULL
## to change to charcter to organize
hiVec <-answerfor82 <- unique(c(as.character(final_end$V2)))
hiVec
data.class(hiVec)
# length 8
hiNum <- sapply(strsplit(as.character(final_end$V2),""), length)
hiNum 
##### all 8

#8-3 Zip : most frequent value of vector
### changed to vector/ character
####GOOD

zipVar <- c(as.character(data$Zip))
### substr - 1~4 (selecting first four letters)
zipfirst4 <- substr(zipVar, start = 1 , stop = 4)
### finding most frequent value 
tail(names(sort(table(zipfirst4))), 1)

# 8-4 boolean for Descrip
####GOOD
# should have only looked for communications<<< with an 's' but I'll give you credit because I made the same mistake

### lower-cased all letters
lowerDes <- tolower(data$Descrip)
lowerDes[grep("communication", lowerDes)]
###### got 10 cases with "communication," but some of them are 
##### "telecommunication," so not original 
##### [1] "telecommunications"              "telecommunications services"    
##### [3] "telecommunications"              "mass communications data 1673"  
##### [5] "communication services"          "research/communications"        
##### [7] "communications: radio"           "telecommunications"             
##### [9] "earmarked by sbc communications" "communications consulting"    

# 8-5
### first, created another column with first letter of CRPFilerid, which is in alphabet
####GOOD


CRPFilerChar = substr(as.character(data$CRPFilerid), 1, 1)
### created three cases 1- when CRFilerid is N or both amount is greate than 500, 
### descrip : non-missing
valid <- CRPFilerChar == "N" 
valid2 <- data$Amount > 500
valid3 <- data$Descrip != ''
### consolidated into one data frame, and used or/ and operator to get the result I want
tableValid <- data.frame(valid,valid2,valid3)
finalValid <- tableValid[tableValid$valid == "TRUE" | tableValid$valid2 == "TRUE" & tableValid$valid3 == "TRUE", ]
finalValid
### got 12392 observations

# Extra Credit
### lowered the data$descrip, splited based on chracters, and unlisted all.
####GOOD +1

extra1 <- unlist(strsplit(tolower(as.character(data$Descrip)), ""))
table(extra1)
##### e is used the most (35122), followed by n (28286)

# Q9
####GOOD


### get stateVAr to check the unique state,
### but since 6 removes few state variable, let's run again
#convert <- levels(as.factor(data$State))
### convert it to .csv file by using the forloop
convert <- levels(as.factor(data$State))
convert
## data delete => 1:58, after data cleansing => 1:54
for(i in 1:54){
  write.csv(data[data$State == convert[i],],paste0(convert[i], ".csv"))
}




