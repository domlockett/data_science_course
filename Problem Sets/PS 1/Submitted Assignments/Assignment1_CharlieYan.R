#GRADE: 12/15 You had problems answering all parts of each question. Explanation in comments of each problem
####GOOD
my_data <- read.csv(file.choose())
my_data <- read.csv('Expends2002.csv')
#1 Open the dataset as a dataframe. This dataframe should have the following properties: a) The column
#names should match the column names in the original dataset. b) The row names should correspond to
#the variable ID in the original dataset.
######GOOD
my_data <- data.frame(my_data) #Open the dataset as a dataframe
colnames(my_data) #list column name
my_data$ID #list row ID name

#2 Change the variable name TransID to Useless.
#Good
dimnames(my_data)[[2]][3]<-"UseLess" 

#3 Remove the variables Useless, and Source from the dataframe.
####GOOOD

my_data <-subset(my_data, select = -c(UseLess, Source))
colnames(my_data)
#4 Change the variable EntType to a factor. How many levels does this variable have?
####GOOD
my_data[, 'EntType']<-as.factor(my_data[, 'EntType']) #change variable to factor
str(my_data[, 'EntType']) #find the level
#Factor w/ 8 levels

#5 The variable State contains several obvious errors, as it includes non-existent state codes.
#-Identify observations that have non-existent state codes.
#-Write a script to recode these observations. Use the additional information in the dataset (candidate
#name, city, zip code) to correctly identify each state.

######-1
#######your function identified all of the incorrect state codes, however, you did not identify the state using other contextual information and replace them with the correct state. i.e LL == IA; ZZ == VI ; St == FL

index=0 
for (i in my_data$State ){
  if(!(i %in% state.abb)&&i!='DC'&&i!='GU'){#check if the input is a valid statecode
    print(i) #print element
    print(index) #print location
  }
  index=index+1
}
index=0 #record location in loop


#6 Remove all observations from the dataset where the variable State is missing. Report the number of
#####GOOD 
#but you should have looked for contextual information instead of removing the LL and ZZ and St values. 
#observations after removing missing values.
index=0
for (i in my_data$State){ #filter all possible state code, even the incorrect
  if(!(i %in% state.abb)&&i!='DC'&&i!='GU'&&i!='AS'&&i!='LL'&&i!='ZZ'&&i!='VI'&&i!='St'){
    print(index)
    my_data <- subset(my_data, my_data$ID!=index)
  }
  index=index+1
}
index=0

#7
####GOOD
class(my_data[, 'Zip'])
my_data[, 'Zip']<-as.numeric(levels(my_data[, 'Zip']))[(my_data[, 'Zip'])]
mean(my_data[, 'Zip'],na.rm=TRUE)
#48217329

#8 Change the variable Zip into a numeric. Be sure to document what you do with missing cases. What is
# the mean of this variable?
#-Change the variable Zip into a numeric. Be sure to document what you do with missing cases. What is
#the mean of this variable?

####GOOD

class(my_data$Descrip)
my_data$DescripW <- sapply(strsplit(as.character(my_data$Descrip), " "), length)
median(my_data$DescripW)

#-A variable containing the numeric portion of CRPFilerid. This variable should be of length 8 for
#all observations. What is the number of unique values of this variable?

##### -1/2
#you were supposed report the number of unique values not just have a list. second line should have been:length(unique(my_data$NumCRPFilerid))
my_data$NumCRPFilerid<-substr(my_data$CRPFilerid,2,9)
unique(my_data$NumCRPFilerid)


#-A vector containing the first four digits of Zip. What is the most frequent value of this vector?

#####-1 
#####You misunderstood the question. This asks to make the Zip variable into four characers. in the second line of code you split the F4Zip variable into single numbers whereas you should have used the 4 digit values and found that which was most occuring. Look at the second line I inserted which is commented out:

my_data$F4Zip<-as.numeric(substr(as.character(my_data$Zip), 1,4))
#>>> names(which(max(table(my_data$F4Zip))==table(my_data$F4Zip)))<<< had you run these two lines you would have recovered the correct answer

my_data$F4Zip<-sapply(as.character(my_data$F4Zip),strsplit,split=character(0))
my.vector<-as.integer(unlist(my_data$F4Zip))
sort(table(my.vector),decreasing=TRUE)[1:3]

#-A boolean indicating whether the Descrip variable contains the word “Communications” REGARDLESS OF CAPITALIZATION. Report the number of TRUE values in this boolean.

##### -1/2 
####you were asked to report the number of true values in my_data$BooleanDescrip: sum(my_data$BooleanDescrip) had you done this you would have got the correct value of 9 occurances

class(my_data$Descrip)
my_data$BooleanDescrip<-grepl("communications", my_data$Descrip ,ignore.case = TRUE)


#-A variable indicating that either CRPFilerid is “N” or that BOTH Amount is greater than 500
#and Descrip is non-missing. Report the number of TRUE values.
#####GOOD


my_data$Both<-substr(my_data$CRPFilerid,1,1)=="N"|(my_data$Amount>500&my_data$Descrip!='')
#loop to count true
index=0
for (i in my_data$Both){ #filter all possible state code, even the incorrect
  if(i==TRUE){
    index=index+1
  }
}
print(index) #19997
index=0

#-EXTRA CREDIT: A variable that provides the most common letter in the Descrip variable.
#####This did not run for me so if you can report the most common letter I can give you the extra point
my_data$MostCommonDescrip<-sapply(as.character(my_data$Descrip),strsplit,split=character(0))
my_data$MostCommonDescrip
my_data$MostCommonDescrip<-names(sort(summary(as.factor(my_data$MostCommonDescrip)), decreasing=T)[1])

#9 Write a script that subsets the data by state, and writes out a unique CSV file for each subset, where
#each file has a unique (and meaningful) name (hint: look at by() function).
########GOOD
usStates<-c(state.abb,'DC','GU')

for(i in usStates){
  print(i)
  curr<-subset(my_data, State==i ) #categorize data by their State element
  write.csv(curr,paste0("", i, ".csv"),row.names = FALSE) #unique names
  rm(curr) #remove so the next iteration can use the same variable and do not occupy extra memories
}


