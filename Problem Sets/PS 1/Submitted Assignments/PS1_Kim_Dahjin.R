## Political Data Science
## Problem set 1
## Jin Kim (Dahjin Kim)
# GRADE: 16/15
rm(list=ls())

##1. Open the dataset as a dataframe. 
##This dataframe should have the following properties: 
##a) The columnnames should match the column names in the original dataset. 
##b) The row names should correspond tothe variableIDin the original dataset.
#Good
expenditure <- read.csv("Expends2002.csv")


##2. Change the variable name TransID to Useless.
#Good
names(expenditure)[names(expenditure) == "TransID"] <- "Useless"


##3. Remove the variables Useless, and Source from the dataframe.
#Good
expenditure <- within(expenditure, rm("Useless", "Source"))


##4. Change the variable EntType to a factor. How many levels does this variable have?
#Good
entType <- as.factor(expenditure$EntType)
nlevels(entType)


##5. The variable State contains several obvious errors, as it includes non-existent state codes.
##5-1. Identify observations that have non-existent state codes.
#Good


territory.abb <- c("AS", "GU", "MH", "FM", "MP", "PW", "PR", "VI", "DC")
state.territory.abb <- c(state.abb, territory.abb)
check <- expenditure$State %in% state.territory.abb
expenditure$State[!check]


##5-2. Write a script to recode these observations. 
##Use the additional information in the dataset (candidate name, city, zip code) 
##to correctly identify each state.
#Good

expenditure[expenditure$State=="St", ] #to pull the observation
expenditure[expenditure$State=="ZZ", ]
expenditure[expenditure$State=="LL", ]
expenditure$State[expenditure$State=="St"] <- "DC"
expenditure$State[expenditure$State=="ZZ"] <- "VI"
expenditure$State[expenditure$State=="LL"] <- "IA"


##6. Remove all observations from the dataset where the variableStateis missing. 
#Good

##Report the number of observations after removing missing values.
notmissvalues <- (expenditure$State != "  ") #create a logical vector for values not missing
expenditure <- expenditure[notmissvalues, ] #subset by using the logical vector
nrow(expenditure)


##7. Change the variable Zip into a numeric. 
##Be sure to document what you do with missing cases. 
##What is the mean of this variable?
#GOod

zip <- as.numeric(as.character(expenditure$Zip), na.rm=T)
mean(zip, na.rm = T)


##8. Create new variables that contain the following information (you will be making several variables),
##and answer the questions:
##8-1. The number of words in the Descrip variable. What is the median value of this new variable?
#Good


wordcount <- function(x) {
  length(unlist(strsplit(as.character(x), "\\W+")))
}
words <- sapply(expenditure$Descrip, wordcount)
sum(words)
median(words)


##8-2. A variable containing the numeric portion of CRPFilerid. 
##This variable should be of length 8 for all observations. 
##What is the number of unique values of this variable?
#Good


#First, check if all observations are of equal length
CRPF <- as.character(expenditure$CRPFilerid)
countCRPF <- nchar(CRPF)
checkCRPF <- countCRPF != 9
sum(checkCRPF) 

#Subsetting only the numeric portion
varCRPF <-substr(CRPF, 2, 9)

#Checking the number of unique values
uniqueCRPF <- unique(varCRPF)
length(uniqueCRPF)


##8-3. A vector containing the first four digits of Zip. What is the most frequent value of this vector?
#Good


#keeping leading zeros
zip2 <- formatC(zip, width = 5, format = "d", flag = "0")

#subset the first four digits
zip4 <- substr(zip2, 1, 4)

#most frequent value
names(sort(summary(as.factor(zip4)), decreasing = T)[2]) 
#I'm indexing the second element here because R combines less levels into one '(Other)' level, 
#which contains 12499 elements. So if I index the first element, (Other) will show.
#To avoid this problem, I'm indexing the second element in the function, which will show the true most frequent value.


##8-4. A boolean indicating whether the Descrip variable contains the word ¡°Communications¡± 
##REGARDLESS OF CAPITALIZATION. Report the number of TRUE values in this boolean.
#Good


#create a character vector with lowercases
lowerDescrip <- tolower(as.character(expenditure$Descrip))

#installing stringr packages
install.packages("stringr")
library(stringr)

#creating a boolean vector and counting TRUE values
booDescrip <- str_detect(lowerDescrip, "communications", negate = F)
sum(booDescrip)

#OR you can use grepl() in base R
booDescrip2 <- grepl("communications", expenditure$Descrip, ignore.case = T)
sum(booDescrip2)

##8-5. A variable indicating that either CRPFilerid is ¡°N¡± or that 
##BOTH Amount is greater than 500 and Descrip is non-missing. 
##Report the number of TRUE values
#Good


#Subsetting the letter in CRPFilerid & checking for missing values
CRPF <- as.character(expenditure$CRPFilerid)
ncCRPF <- substr(CRPF, 0, 1)
expenditure$Descrip[expenditure$Descrip==""] <- NA

#logical vectors
x <- ncCRPF == "N"
y <- (expenditure$Amount > 500 & !is.na(expenditure$Descrip))
xory <- (x | y)

#number of TRUE values
sum(xory)


##8-6. EXTRA CREDIT: A variable that provides the most common letter in the Descrip variable.
#Good +1


#split characters
descrip <- as.character(expenditure$Descrip)
descrip2 <- unlist(strsplit(descrip, split = ""))

#delete non-alphabets and turn everything to lowercase
descrip2 <- tolower(gsub("[^[:alpha:]]", NA, descrip2))
descrip2 <- na.omit(descrip2)

#create a variable for most common letter in Descript
most.common.letter.Descript <- names(sort(summary(as.factor(descrip2)), decreasing = T))[1]
most.common.letter.Descript


##9. Write a script that subsets the data by state, and write out a unique CSV file for each subset, 
##where each file has a unique (and meaningful) name (hint: look at by() function).
#Good


state.subset <- by(expenditure, expenditure[, "State"], subset)
statename <- function(x) {
  write.csv(state.subset[[x]], file = paste0(names(state.subset[x]), ".cvs"), row.names = F)
}
lapply(1:length(state.subset), statename)
