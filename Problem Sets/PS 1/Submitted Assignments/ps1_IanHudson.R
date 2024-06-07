rm(list=ls())
#GRADE:15/15
# part 1
#Good


df <- read.csv(file='Expends2002.csv') # read in file
row.names(df) <- df$ID # set row name equal to the row id

# part 2
#Good


colnames(df)[3] <- "Useless" # change column name

# part 3
#Good


df <- df[,c(-3, -length(df))] # remove the 3rd and last columns from the data frame

# part 4
#Good


df$EntType<-as.factor(df$EntType) # convert column to a factor
print(paste('The EntType variable has', nlevels(df$EntType),'levels'))

# part 5
######-1 
####If you would have manually looked at the non.existent.state rows you would have found contextual information other than zip that could have told you where they were: St==DC, LL==VI, ZZ==IA


non.existent.state<-df[df$State=='LL' | df$State=='ZZ' | df$State=='St',] # observations with non-existent state codes
non.existent.state

for(i in 1:length(non.existent.state$State)){
  mask1 <- df$ID != non.existent.state$ID[i] # don't want to choose the same entry
  mask2 <- (df$City == non.existent.state$City[i]) & (non.existent.state$City[i] != '') # if the cities are the same and we are not searching for an empty string
  mask3 <- (df$Zip == non.existent.state$Zip[i]) & (non.existent.state$Zip[i] != '') # if the zips are the same and we are not searching for an empty string
  mask4 <- (df$Candid == non.existent.state$Candid[i]) & (non.existent.state$Candid[i] != '') # if the candidate ids are the same we are not searching for an empty string
  mask5 <- (df$CRPRecipname == non.existent.state$CRPRecipname[i]) & (non.existent.state$CRPRecipname[i] != '') # if the recipients are the same we are not searching for an empty string
  
  masks_combined <- mask1 & ( mask2 | mask3 | mask4 | mask5) # need to not be the same entry and have at least one property in common
  
  print(non.existent.state[i,])
  print('potentially replace with:')
  candidate.state.replacement<-df[masks_combined,]
  print(candidate.state.replacement)
}

# all the matches with common entries were non-valid. ie. Zip=00000 or Zip=XXXXX. Therefore didn't change any state codes
# Instead, change the all the state codes to na
df[df$State=='LL' | df$State=='ZZ' | df$State=='St',]$State <- NA

# part 6
#Good


df<-df[df$State != "  " & !is.na(df$State),] # keeps all entries where state is not missing or not equal to NA
df.remaining.entries = length(df[,1])
print(paste("there are", df.remaining.entries, "entries left"))

# part 7
#Good


df<-transform(df, Zip = as.numeric(as.character(Zip))) # change Zip variable to an integer.
df<-subset(df, !is.na(Zip)) # All entries with Zips that are missing are removed from the data frame
mean.of.zips<-mean(df[,"Zip"])
print(paste('the mean of the zips=', mean.of.zips))

# part 8
#Good

number.of.words<-lengths(strsplit(as.character(df$Descrip), split= " ")) # split up the words in each descrip entry based on where spaces are, then get the size of that list
df[,"words.in.Descrip"] <- number.of.words # create new column
median.num.of.words<-median(df[,"words.in.Descrip"]) 
print(paste('The median number of words in Descrip is:', median.num.of.words))


numeric.portion.of.crp<-substr(df$CRPFilerid, start=2, stop=9) # get rid of the first character
df[,"numeric.portion.of.crp"]<-numeric.portion.of.crp
unique.values<-length(unique(df[,'numeric.portion.of.crp'])) # create a list that has no duplicates and get the length of that list
print(paste('there are:', unique.values, 'unique values'))

# assuming that we are getting rid of the leading zeros of Zip which happens when we convert to a numeric
#Good

first.four.digits.of.zip<-substr(as.character(df$Zip), 1, 4) # convert the zip numeric to a string so we can use substr
df[,'four.dig.of.zip']<-first.four.digits.of.zip

# getting the mode of zip
zip.array<-df[,'four.dig.of.zip']
possible.values<-unique(zip.array) # create list of all unique values of zip
find.matches<-match(zip.array, possible.values) # creates list of the same size as the original list Each element tells you which index it matches up with in the possible.values list
count.matches<-tabulate(find.matches) # creates list where each index contains the number of times the value of the index shows up in find.matches
i<-which.max(count.matches) # finds index that has the max value
zip.mode<-zip.array[i] # get which zip value is at that index

print(paste('the most frequent value of the zip is:', zip.mode))

#Good

replace.comm<-gsub('communications', '',ignore.case=T, x=df$Descrip) # take out the word communications from all the descriptions
length.changed<-lengths(strsplit(replace.comm, '')) != lengths(strsplit(as.character(df$Descrip), '')) # get the number of characters of each item in the list. If length changed, then we know that communications was removed from the string
print(paste('there are ', sum(length.changed), ' entries that have communications in the description'))

check<-substr(df$CRPFilerid,1,1)=='N' | (df$Amount>500 & !df$Descrip=='') # all entries that satisfy the conditions
print(paste('there are ', sum(check), 'entries that match the condition'))
df[,'multiple.check']<-check # add whether entry meets conditions to the dataframe


# part 9
#Good


dir.create(file.path(getwd(), 'output'), showWarnings = FALSE) # create folder for all outputs
write.to.file<-function(data){
  name<-paste(data[1,'State'], '-info.txt', sep='') # creating name of txt file for each state

  write.csv(data, file.path(getwd(), 'output', name)) # write to the file based on state
}

by(data=df[,], INDICES=df$State, FUN=write.to.file) # subset the dataframe by the state code and write to the file for each subset

# extra credit
#Good 


descrips<-as.character(df$Descrip)
max.count<-0 # keep current max value of number of characters
max.letter<-NA # keep current most common character
for (ascii in seq(97, 122)){
  letter<-intToUtf8(ascii)
  # go through each letter is ascii values from 97 to 122 (all lowercase values)
  original.length <- sum(lengths(strsplit(descrips, ''))) # get original length of the sum of all the descrip variables
  new.descrips<-gsub(letter, '', ignore.case=T, x=descrips) # take out all of the certain letter the loop is on
  new.length <- sum(lengths(strsplit(new.descrips, ''))) # get new length of the sum of all the descrip variables after removing the character
  count<-original.length-new.length # how many characters were there
  if(count>max.count){
    # if this character is more frequent than the previously most frequent, then make this the new most
    max.count<-count
    max.letter<-letter
  }
}

print(paste('the most common letter was', max.letter, 'with', max.count, 'letters'))
