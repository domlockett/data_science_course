#Alexander Newman ID 450781 
###GRADE: 16/15
#Problem Set 1

#PROBLEM 1
####GOOD


#open the Expends2002 file and save it into dataframe df
rm(list=ls())
df<-read.csv("Expends2002.csv")

#PROBLEM 2
####GOOD


#rename TransID to Useless
colnames(df)[which(colnames(df)=="TransID")]<- "Useless"

#PROBLEM 3
####GOOD

#Remove Useless and Source from df
df$Useless<-df$Source<-NULL

#PROBLEM 4
####GOOD

#Change EntType to a factor
df$EntType<-factor(df$EntType)
#calculate number of levels of EntType
print(length(levels(df$EntType)))

#PROBLEM 5
####GOOD

#discover which entries have an invalid state name/code by finding everything without state abbreviation, Guam, American Samoa, Virgin Islands, DC, or no info 
statenames<-c(state.abb,"DC","VI", "AS", "GU", "  ")
df[!df$State%in%statenames,]
#Set Thurman for Congress' state as DC
df$State[8994]<- "DC"
#Set Republican Party of Virgin Islands as VI
df$State[12333]<-"VI"
#Set Republican party of Iowa as IA
df$State[12983]<- "IA"

#PROBLEM 6
####GOOD

#remove unlisted state codes
df<-subset(df, df$State!="  ")
#Calculate number of observations
print(length(df$State)) #19912

#PROBLEM 7
####GOOD

#Create numeric zip codes
zips<-as.numeric(as.character(df$Zip))
#remove all NA values from zips
zips<-na.omit(zips)
#Calculate mean zip code (it is very high because I left in the zip+4 codes)
print(mean(zips))

#PROBLEM 8
####GOOD

#Calculate number of words in descrip variable for each entry
wordcount<-NULL
for(i in df$Descrip){
  wordcount<-c(wordcount,sapply(strsplit(i, split= " "), length))
}
#calculate median number of words in descrip
print(median(wordcount))
#Obtain numeric portion of CPRFilerid
CPRnumeric<-substr(df$CRPFilerid, start=2, stop=9)
#find the number of unique CPRFilerids
print(length(unique(CPRnumeric))) #2243
#get the first four digits of Zip, first using string so no initial 0s are lost before converting to numeric
firstfour<-substr(as.character(df$Zip),start=1, stop=4)
#convert to numeric
firstfour<-as.numeric(firstfour)
firstfour<-na.omit(firstfour)  #Remove the NA Values


#calculate the mode of firstfour
#first, create a factor out of the digits to create categorical variable
firstfourfactor<-factor(firstfour)
#create a dataframe with a frequency column
ziptable<-as.data.frame(table(firstfourfactor))
#output the most common value in the ziptable, removing the levels output
print(ziptable$firstfourfactor[which.max(ziptable$Freq)], max.levels=0)

#create boolean vector that checks whether descrip has communications
#convert all to lowercase
####GOOD

lowercase<-tolower(df$Descrip)
#Create a vector of the booleans
hascom<-NULL
for(phrase in lowercase){
  hascom<-c(hascom, (grepl("communications",phrase)))
}
#find the number of TRUE in hascom
print(sum(hascom))

#Create variable indicating CPRFilerID is N or both amount is >500 and descrip exists
####GOOD

df$filerIdFirstChar<-substr(df$CRPFilerid, start=1, stop=1)
weirdvariable<-(df$filerIdFirstChar=="N" | (df$Amount>500 & df$Descrip!=""))
#report number of true values
print(sum(weirdvariable))

#EXTRA CREDIT
####GOOD +1
#find most common letter in the descrip variable
#create a really long string of every word in descrip with no spaces
longstring<-""
for (sentence in df$Descrip){
  longstring<- paste0(longstring, gsub(" ", "", tolower(sentence)))
}
#make a vector of every single character, convert it into frequency table
longvector<-unlist(strsplit(longstring, split="" ))
longfactor<- factor(longvector)
longtable<-as.data.frame(table(longfactor))
#output the most frequently occuring letter
print(as.character(longtable$longfactor[which.max(longtable$Freq)]))

#PROBLEM 9
####GOOD

#subset every state
statesubs<-by(df, list(df$State), subset)
#For whatever reason the statesubs keep LL and ZZ and "  " as options even though there are no states with those names so I will just remove them here
statesubs$ZZ<-NULL
statesubs$`  `<-NULL
statesubs$LL<-NULL
statesubs$St<-NULL
#initiate file name variables
filenametowrite<-""
statetitle<-""
#for loop that writes a series of csv files with unique descriptive names to the folder statedata
#make sure to title the filenametowrite section to the folder you want the files to go in
for(stateframe in statesubs){
  statetitle<-paste0(stateframe$State[1], "_data")
  filenametowrite<-paste0("", statetitle, ".csv")
  write.csv(stateframe,filenametowrite)
}

