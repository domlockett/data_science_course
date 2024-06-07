####GRADE: 14/15
# 1: reading in the data and setting row names and col names
####GOOD

setwd("C:/Users/oyina/src/senior 2019-2020/spring_2020/political_data_science/problem_sets/ps1")
#df = read.table("Expends2002.txt", header = T, sep = ",", row.names = "ID")
df= read.csv('Expends2002.csv')

#2
#####GOOD

names(df)[names(df) == "TransID"] = "Useless"

#3
#GOOD

toRemove = which(colnames(df) %in% c("Useless", "Source"))
df = df[-toRemove]
# df = df[,-c("Useless", "Source")]

#4 
#### -1/2
#You should have reported the number of observations: length(as.factor(df$EntType)) ==8

#but no need for lapply. as.factor(df$EntType)
df["EntType"] = lapply(df["EntType"], as.factor)

# 5: remane nonsese state codes and 
#### -1/2
### you should have  looked at the whole row and gotten more context for instance the variable 'Pacshort' shows that the "LL" state code is Iowa. 


states = read.csv("data.csv", header = T)
states = states[c("Code")]
uniqueStates = unique(df$State)
uniqueStates
toRemove = c("St", "ZZ", "LL")
toFix = df$State %in% toRemove
df[toFix,][c("City", "Zip", "Candid", "State")]
# fixing state codes for data with nonsense codes
df["634397","State"] = "VI"
df["368552","State"] = "  " # could not figure out
df["162479", "State"] = "  " # could not figure out
# df[toFix,][c("City", "Zip", "Candid", "State")]


# 6: remove rows with missing state codes
####GOOD

toKeep = !(df$State == "  ")
df = df[toKeep,]
nrow(df)
# number of rows after removing data with missing rows: 19910

# 7
####GOOD


df["Zip"] = lapply(df["Zip"], as.numeric)
forZipMean = df[!(is.na(df$Zip)), "Zip"]
zipMean = mean(forZipMean)
zipMean
# the mean of Zip is 48214902

#8
#####GOOD


# descrip vairable
# split the string by spaces
splitDescrip = strsplit(as.character(df$Descrip), " ")
head(splitDescrip)
# get the lengths of all of the lists of strings
splitDescripLengths = lengths(splitDescrip)
splitDescripLengths[1:15]
# get the median of the lists of strings
medianDescrip = median(splitDescripLengths)
medianDescrip
# the median number of words in the Descrip variable is 2

# numeric portion of CRPFilerid
####GOOD


df$CRPFilerid[1:15]
numCRP = as.numeric(substr(df$CRPFilerid, start = 2, stop = 9))
numUniqueCRP = length(unique(numCRP))
numUniqueCRP
# the number of uniqe CRPFilerid numbers is 2243

# zip alterations
# cast the zips as strings and get the sub zip 
####GOOD



stringZips = as.character(df$Zip)
subStringZips = substr(stringZips, start = 1, stop = 4)
# wild that there is no built in mode function in R>>> I agree. pretty dumb.<<<
# get all of the unque zips
uniqueZips = unique(subStringZips)
# number of each unique zip that appears in the full list of zips
numEachUniqueZip = c()
for (zip in uniqueZips){
  numEachUniqueZip = c(numEachUniqueZip, length(which(subStringZips == zip)))
}
# most frequently appearing zip
numModeZip = max(numEachUniqueZip)
# index of most frequently appearing zip
modeIndex = which(numEachUniqueZip == numModeZip)
modeZip = subStringZips[modeIndex]
modeZip
# the most frequent value of this vector is 2000

# descrip variable contain the word "communications"
# turn all strings to lowercase
#####GOOD



splitDescrip = tolower(df$Descrip)
# boolean: true if string contains "communicaitons"
commOrNah = sum(grepl("communications", splitDescrip))
sum(commOrNah)
# the number of true values is 9

# last booleans exercise
# perform filtering on vectors
#####GOOD


crpFileridIsN = substr(df$CRPFilerid, start = 1, stop = 1) == "N"
amountOver500 = df$Amount > 500
descripIsPresent = splitDescripLengths
descripIsPresent[descripIsPresent == 0] = FALSE
descripIsPresent[descripIsPresent != 0] = TRUE
descripIsPresent = as.logical(descripIsPresent)
# combine and sum the boolean values
sum(crpFileridIsN | (amountOver500 & descripIsPresent))
# the number of true values is 12391

#9
#GOOD


cwd = "C:/Users/oyina/src/senior 2019-2020/spring_2020/political_data_science/problem_sets/ps1/"
it = by(df, factor(df$State), data.frame)
for (frame in it) {
  frame
  filename = paste(cwd, frame$State[1], ".csv", sep = "")
  print(filename)
  write.csv(frame, file = filename)
}
