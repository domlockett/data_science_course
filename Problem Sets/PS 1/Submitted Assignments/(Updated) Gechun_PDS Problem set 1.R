#16/15

#1. Open the dataset as a dataframe
#Good
mydata <- read.csv('Expends2002.csv')

#2. Change the variable name TransID to Useless
#Good
names(mydata)[3] <- "Useless"

#3. Remove the variables Useless, and Source from the dataframe
#Good
mydata$Useless <- NULL
mydata$Source <- NULL

#4. Change the variable EntType to a factor
#Good
mydata$EntType <- factor(mydata$EntType)

#How many levels does this variable have? 8 levels ("" "CAN" "CCM" "COM" "IND" "ORG" "PAC" "PTY")
#Good
levels(mydata$EntType)

#5. Identify observations that have non-existent state codes
#Good
mydata$State <- factor(mydata$State)
levels(mydata$State)
nonexisstate1 <- subset(mydata, State=="LL")
nonexisstate2 <- subset(mydata, State=="St")
nonexisstate3 <- subset(mydata, State=="ZZ")
nonexisstate1
nonexisstate2
nonexisstate3

#Recode these observations
mydata$State[mydata$State == "LL"] <- "IA"
mydata$State[mydata$State == "St"] <- "DC"
mydata$State[mydata$State == "ZZ"] <- "VI"

#6. Remove all observations from the dataset where the variable State is missing
#Good
mydata$State[mydata$State == "  "] <- NA
mydata <- na.omit(mydata, cols="State")

#Report the number of observations after removing missing values
nrow(mydata)

#7. Change the variable Zip into a numeric.
#Good
zip <- as.numeric(as.character(mydata$Zip),na.rm=T)
zip
#What is the mean of this variable?
mean(zip, na.rm = T)


#8. (1)create a new vector "descrip" storing the Descrip variable
descrip <- mydata[ , "Descrip"]
descrip

#calculate the number of words in the Descrip variable and the median value of this new variable
#Good
words <- sapply(mydata$Descrip, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
sum(words)
median(words)

#(2)create a new vector "crpf" storing the numeric portion of CRPFilerid variable
#Good
crpf <- substr(mydata[ , "CRPFilerid"], 2,9)

#calculate the number of unique values of this variable
length(unique(crpf))

#(3)create a vector containing the first four digits of Zip
#Good
four.zip <- substr(mydata[ , "Zip"], 1,4)
four.zip

#What is the most frequent value of this vector (Answer: "2000")
summary(as.factor(four.zip), decreasing=T)

#(4)write a boolean indicating whether the Descrip variable contains the word “Communications” REGARDLESS OF CAPITALIZATION
#Good
y <- grepl("communications", mydata$Descrip, ignore.case = T)

#report the number of TRUE values in this boolean (The number is 9)
sum(y)

#(5)create a variable indicating that either CRPFilerid is “N” or that BOTH Amount is greater than 500 and Descrip is non-missing
#Good
a <- grepl("N", mydata$CRPFilerid)
sum(a)
b <- mydata$Amount > 500
sum(b)
mydata$Descrip[mydata$Descrip == ""] <- NA
c <-!is.na(mydata[ , "Descrip"])
sum(c)
my.variable <- (a==T | (b==T & c==T))
sum(my.variable)


#Exrea Credit: create a variable that provides the most common letter in the Descrip variable (Answer: The most common letter is "e")
#Good
letter <- strsplit(as.character(mydata$Descrip), "")
z <- unlist(letter)
names(sort(summary(as.factor(z)), decreasing=T))

#9. Write a script that subsets the data by state, and writes out a unique CSV file for each subset
#Good
subsetdata <- by(mydata, mydata[ , "State"], subset)

lapply(1:length(subsetdata), function(i) write.csv(subsetdata[[i]], 
                                                file = paste0(names(subsetdata[i]), ".csv"),
                                                row.names = FALSE))



