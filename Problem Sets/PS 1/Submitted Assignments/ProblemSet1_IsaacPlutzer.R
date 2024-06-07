#Isaac Plutzer
##GRADE: 15.5/15
##Your code is very well commented and clean
rm(list = ls())

#1
####GOOD

expends2002 = read.csv("Expends2002.csv",stringsAsFactors = F) #Path will need to be changed for grading >>Thanks lol:)<<
#expends2002 = data.frame(lapply(expends2002,as.character), stringAsFactors = F) # Factors = Bad
rownames(expends2002) = expends2002$ID

#2
####GOOD

colnames(expends2002)[colnames(expends2002) == "TransID"] = "Useless"

#3
####GOOD


expends2002 = subset(expends2002, select = -c(Useless, Source))
#colnames(expends2002)


#4
####GOOD

expends2002$EntType = as.factor(expends2002$EntType)
nlevels(expends2002$EntType) # Returns 8

#5
####-1/2
#if you would have simplified this code and just manually looked at the wacky code you could have found that codes like LL, ZZ and St are DC or FL, VI, and IA relatively.


#I manually added "  " as an allowed code because we aren't concerned with these at this point.
#Apparently AS is a valid state code for American Samoa... manually added that one too
allowedCodes = c("  ","AS","AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID", "IL","IN","KS","KY","LA","MA","MD","ME","MH","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY", "OH","OK","OR","PA","PR","PW","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
wackyStates = expends2002[!(expends2002$State %in% allowedCodes),]
nrow(wackyStates) #Since there's only 3 of these I'll do them one by one in a for loop for simplicity...
for (entry in 1:nrow(wackyStates)) {
  # The following line resets the entry's state to the state code of the first 
  # entry in expends2002 with a matching zipcode that also has an allowed State code.
  #print(expends2002[(expends2002$Zip %in% wackyStates[entry]$Zip) & (expends2002$State %in% allowedCodes),]$State[1])
  wackyStates[entry,]$State = expends2002[(expends2002$Zip == wackyStates[entry,]$Zip) & (expends2002$State %in% allowedCodes),]$State[1]
}
# The last entry looks like a refund, and has no identifiable information 
# in the entry to help find the state, so I will delete the state ID
wackyStates[!(wackyStates$State %in% allowedCodes),]$State = '  '
# Update these in the orignal dataframe
expends2002[!(expends2002$State %in% allowedCodes),] = wackyStates

#6
####GOOD

allowedCodes = subset(allowedCodes, allowedCodes != '  ') #removing this as an allowed code.
expends2002 = expends2002[expends2002$State %in% allowedCodes,]
nrow(expends2002) #19911 entries remaining

#7
####GOOD

expends2002$Zip = as.numeric(expends2002$Zip) #Turns non-number values into NA
mean(expends2002[!is.na(expends2002$Zip),]$Zip)

#8
#Number of words in descrip variable
####GOOD


expends2002$DescripLength = unlist(lapply(strsplit(expends2002$Descrip,' '),length))
median(expends2002$DescripLength)

#Numeric Portion of CRPFilerId
####GOOD
expends2002$CRPFilerIDNumeric = gsub("[^0-9]","",expends2002$CRPFilerid)
length(unique(expends2002$CRPFilerIDNumeric))

#First 4 digits of zip
####GOOD

expends2002$ZipFirstFour = substring(expends2002$Zip,1,4)
sort(table(expends2002$ZipFirstFour),decreasing = T)[1] #Will return the most common, and the number of times it occured

#Boolean for Containing 'Communications'
####GOOD

expends2002$CommunicationsBool = grepl('communications',expends2002$Descrip,ignore.case = T)
sum(expends2002$CommunicationsBool)

#Boolean for whatever these entries are supposed to be
expends2002$NBool = (substr(expends2002$CRPFilerid,1,1) == 'N') | ((expends2002$Amount > 500) & (expends2002$Descrip != ''))
sum(expends2002$NBool)

#Most common letter in Descrip (Assuming case sensitive and space doesn't count)
####GOOD +1
sort(table(unlist(strsplit(expends2002$Descrip,''))[unlist(strsplit(expends2002$Descrip,'')) != ' ']),decreasing = T)[1]
# ^ Returns the most common character and the number of times it was used.

#9
#GOOD
by(expends2002,expends2002$State,
   function(x) {
     write.csv(x,paste("~/PDS/ProblemSet1/",x$State[1],".csv",sep = '')) #Change this path if needed
   }
   )
