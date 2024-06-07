#Problem 1
###GRADE: 12.5/15
#Good
Expends2002 <- read.csv("Expends2002.csv")

#Problem 2
#####Good

colnames(Expends2002)[3] <- "Useless"

#Problem 3
#####Good

Expends2002$Useless <- NULL
Expends2002$Source <- NULL

#Problem 4
####-1/2 
#### You were asked to report the number of factors: length(unique(as.factor(Expends2002$EntType))) =8

as.factor(Expends2002$EntType)

#Problem 5
#####good

unique(Expends2002$State)
#Returns list of states in the dataset. Identified St, ZZ, LL to be non-valid state codes. GU, VI, AS are U.S. Territories so I ignored them.
wrongstate<-subset(Expends2002,State=="St" | State=="ZZ" | State=="LL")
#Pacshort value for "LL" state indicates Iowa -> recode for IA
Expends2002$State[Expends2002$State=="LL"] <- "IA"
#CRPRecipname for "ZZ" state indicates Virgin Islands -> recode for VI
Expends2002$State[Expends2002$State=="ZZ"] <- "VI"
#Pacshort value for "St" state indicates Florida because Thurman ran for office in FL in 2002 -> recode for FL
Expends2002$State[Expends2002$State=="St"] <- "FL"

#Problem 6
#####Good

Expends2002<-subset(Expends2002,State!="  ")
#19912 observations after removing entries with no state entry

#Problem 7
#####Good

zip.numeric<-as.numeric(Expends2002$Zip[Expends2002$Zip!="  "])
mean(zip.numeric)
#Mean is stated as 2937.884. I eliminated the empty values in Zip so as to only calculate existent values in the mean.

#Problem 8
#####-1/2
#####what is the median of descrip.wordcount

#adding variable for # of words in the Descrip variable. Use sapply to find length of the strings split by spaces.
descrip.wordcount <- sapply(strsplit(as.character(Expends2002$Descrip)," "),length)

#numeric portion of CRPFilerid
#####Good

num.CRPFilerid<-substr(as.character(Expends2002$CRPFilerid),2,9)
length(unique(num.CRPFilerid))

#first 4 digits of Zip
#####Good

zip.firstfour <- substr(as.character(Expends2002$Zip),1,4)
summary(as.factor(zip.firstfour)) #indicates taht "2000" appears the most with 1561 appearances

#contains "communications"
#####-1/2 
#### You should have reported the number of true values as well!


communications<-grepl("communications",Expends2002$Descrip,ignore.case=TRUE)
length(communications[communications==TRUE])
#variable true if CRPFilerid is "N" OR Amount > 500 AND Descrip non-missing
n.CRPFilerid <- substr(as.character(Expends2002$CRPFilerid),1,1)
test.boolean <- n.CRPFilerid=="N" | (Expends2002$Amount > 500 & Expends2002$Descrip!="")
#EC: find most common letter in the Descrip variable


#Problem 9
##### -1 
#### I realize you successfully created the 51 or so unique potential files but the direction suggest you should have called write.csv. You would have recieved full credit even if you would have commented out this part

#Creates a dataframe for each unique state and calls the new dataframe "Expends2002_" with the "_" being the state code being referenced in the subsetted dataframe
for(i in unique(Expends2002$State)) {
  csvname <- subset(Expends2002,State==i)
  assign(paste0("Expends2002",i),csvname)
}

