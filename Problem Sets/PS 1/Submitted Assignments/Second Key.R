### Mariah Yelenick
### PDS
### Problem Set 1
### 1/16/2020

rm(list=ls()) #clean global environment
#Good
#1
data = read.csv("Expends2002.txt", row.names="ID", stringsAsFactors = FALSE) #read data, create dataframe

#Good
#2
colnames(data)[which(colnames(data)=="TransID")] = "Useless"

#Good
#3
data = data[ , !(names(data) %in% c("Source", "Useless"))]


#4
data$EntType = as.factor(data$EntType)
length(levels(data$EntType)) #8 levels

#5
#GU = Guam
#AS = American Samoa
#VI = Virgin Islands
badStates = c("  ", "LL", "St", "ZZ")
badData = data[data$State %in% badStates,]
data["889335","State"] = "NC"
data["607578","City"] = "Tabb"
data["607578","State"] = "VA"
data["607578","Zip"] = 23693

#6
missingState = data$State %in% badStates
betterData = data[!missingState,]
nrow(data)-nrow(betterData) #89 missing/bad values removed

#7
data$Zip = as.numeric(data$Zip)
#this is weird because zip codes that start with 0 will have it removed
sum(is.na(data$Zip))
#155 NAs introduced where zip codes were not numeric
mean(data$Zip, na.rm = T) #mean zip code is 48207619

#8
data["nWordsDescrip"] = sapply(strsplit(data$Descrip, "\\w+"), length)
#above line splits the string on whitespace using regex into a vector of words
#then takes the length of that vector and saves it in nWordsDescrip
median(data$nWordsDescrip) #median is 2

data["CRPFileridNum"] = substr(data$CRPFilerid, 2, 100)
data$CRPFileridNum[nchar(data$CRPFileridNum) != 8] #no results
data$CRPFileridNum = as.factor(data$CRPFileridNum)
length(levels(data$CRPFileridNum)) #2246 levels

charZip = as.character(data$Zip)
data["first4Zip"] = substr(as.character(data$Zip), 1, 4)
mode(as.factor(data$first4Zip))
uniqueZips = unique(data$first4Zip)
uniqueZips[which.max(tabulate(match(data$first4Zip, uniqueZips)))] 
max(tabulate(match(data$first4Zip, uniqueZips)))
#zip codes starting with 2000 (DC) occurred 1561 times

data["isCommunications"] = grepl("communications", data$Descrip, ignore.case = T)
sum(data$isCommunications) #9 True values

data["notSure"] = data$CRPFilerid == "N" | (data$Amount > 500 & data$Descrip != "")
sum(data$notSure) #7988 True values
allWords = paste(data$Descrip, collapse="") #combine into one big string
allLetters = strsplit(tolower(gsub("[^A-Za-z]", "", allWords)), "")
#^^filters out non-alpha characters, converts to lowercase, and splits into
#vector of one character strings
uniqueLetters = unique(allLetters[[1]])
uniqueLetters[which.max(tabulate(match(allLetters[[1]], uniqueLetters)))] 
max(tabulate(match(allLetters[[1]], uniqueLetters)))
#most popular letter is 'e' which occurred 35302 times in the Descrip column

#9
for(l in levels(as.factor(betterData$State))) {
  write.csv(betterData[betterData$State == l,], file=paste(l, ".csv"))
}

#I tried working with this but couldn't get it to name files in a useful way
#by(betterData, betterData$State, write.csv, file=paste(betterData$State, ".csv"))
