#####TAYLOR DAMANN#########
#########PS1##########
###GRADE: 15/15

#1
#Good


Expends2002 <- read.csv("Expends2002.csv")
head(Expends2002)
row.names(Expends2002)
#Although this is a .txt file, the values are separated by commas.
#The read.csv command does not require any arguments aside from the name of the file
#I'm working with. It automatically makes the first row the column names. 
#However, I specify row.names=NULL because the first row is year. Instead, it assigns
#number to the row names which will correspond with the ID. 

#2
#Good


setnames(Expends2002, "TransID", "Useless")
colnames(Expends2002)
# To change the name of a variable, I use the data.table package because the
#setnames function is easy. In setnames, the arguments are (data, oldName, newName)
#I call the colnames on Expends2002 to make sure that the name has been changed

#3
#Good


Expends2002 <- subset(Expends2002, select = -c(3,21))
colnames(Expends2002)
#To remove variables Useless and Source, I subset the original dataset by subtracting
#the third and 21st columns. I know these variables are columns 3 and 21 because of the
#colnames function I called in #2. I call this function again to check if the variables
#were dropped. 

#4
#Good


typeof("EntType")
Expends2002$EntType <- as.factor(Expends2002$EntType)
str(Expends2002)
#Before I change EntType to a factor, I want to know what type of variable this is. The
#command typeof() shows me that this is a character vector. To make it a factor, I use the
#as.factor function. To make sure it worked and find out how many levels there are, I use
#the structure function. This shows that there are 8 levels to the EntType factor. 

#5
#Good


levels(Expends2002$State)
#I call levels so I can see which area abbreviations are wrong. 
state.abb <- c(state.abb, "DC", "AS", "GU", "MH", "FM", "MP", "PW", "PR", "VI")
#I add some territory abbreviations to the base state abbreviations vector because the data contain those.
exist <- Expends2002$State %in% state.abb
#I tell r to create a vector only of those that exist and those that do not. 
nonexist <- Expends2002$State[!exist]
levels(nonexist)
#Calling levels here lets me see that the wrong false abbreviations are St ZZ LL

#I change each code manually below. The first line pulls all information about the variable.
#I use this information to add the correct abbreviation. 
Expends2002[Expends2002$State=="St", ]
Expends2002$State[Expends2002$State=="St"] <- "DC"

Expends2002[Expends2002$State=="ZZ", ]
Expends2002$State[Expends2002$State=="ZZ"] <- "VI"

Expends2002[Expends2002$State=="LL", ]
Expends2002$State[Expends2002$State=="LL"] <- "IA"

#6
#Good


Expends2002 <- subset(Expends2002, Expends2002$State != "  ")
#I know from looking at the data that missing states actually have a value of "  ", so
#I tell r to subset only when state value is not equal to "  "
dim(Expends2002)
#Calling dimensions shows me that I went from 20000 observations to 19912. 

#7
#Good


#I remove all missing data by subsetting. 
Expends2002 <- subset(Expends2002, (!is.na(Expends2002[,"Zip"])))
#I use a simple function to make this a numeric. 
Zipdata <- as.numeric(as.character(Expends2002$Zip))
#To make sure it is numeric, I use the class() function
class(Zipdata)
#I calculate and print the mean. 
Zipmean <- mean(Zipdata, na.rm=T)
print(Zipmean)

#8

#a 
####GOOD
###Need to recod the number of words in the descrip variable

#First I look at the variable.
head(Expends2002$Descrip)
#I decide to use a function from stringr, so I load that.
library(stringr)
spaces<- str_count(Expends2002$Descrip, '\\w+ ')
#This leaves each observation with one less spaces, so I add one to each
descripWord <- spaces+1
descripWord
#I ask for the median below
medianword <- median(descripWord)
medianword

#b
#Good
#First I look at the variable and realize that the numeric portion
#is the second through ninth character, so I substring. 
head(Expends2002$CRPFilerid)
crpnum <- substr(Expends2002$CRPFilerid, start=2, stop=9)
#I look at the variable t see if this worked. 
head(crpnum)
#I ask r how many unique values of this variable there are.
uniquecrp <- length(unique(crpnum))

#c
#Good
#First I look at the variable
head(Expends2002$Zip)
#I substring the first 4 digits of the zip variable. 
first4 <- substr(Expends2002$Zip, start=1, stop=4)
#I create a function to calculate mode. 
mode <- function(z) {
  uniques <- unique(z)
  uniques[which.max(tabulate(match(z, uniques)))]
}
#This is the modal value of the first 4 digits. 
mode(first4)

#d
#Good
#I start by making all descriptions lowercase and checking if it worked.
lowerDescrip <- casefold(Expends2002$Descrip, upper = FALSE)
head(lowerDescrip)
#I use the grep function to pull all instances of communications 
comms <- grepl("communications", lowerDescrip)
comms
#Because comms is a logical vector, I sum to see the number of True values. 
sum(comms)

#e
#Good
#I make a variable with only the first letter of the CRP id
crp1 <- substr(Expends2002$CRPFilerid, start=1, stop=1) == "N"
#I make a variable where only >500 is true
amount <- Expends2002$Amount > 500
#I need to deal with some missing values.
Expends2002$Descrip[Expends2002$Descrip==""] <- NA
#I create a vector only of nonmissing values. 
nonmiss <- !is.na(Expends2002$Descrip)
sum(nonmiss)
#Now that I've created vectors, I make a simple logical statement. 
logic <- (crp1 | (amount & nonmiss))
#The sum of logic shows how many true values there are. 
sum(logic)

#9
#good
#I start by subsetting the data by state. This creates a list. 
statedata <- by(Expends2002, Expends2002$State, subset)
#I make a function that writes csvs for each state. I use the paste()
#function to create unique file names. 
statefile <- function(x) {
  write.csv(statedata[[x]], paste0(names(statedata[x]),".csv"), row.names=F) 
}
#the lapply function loops through the function to make several csvs. 
lapply(1:length(statedata), statefile)

