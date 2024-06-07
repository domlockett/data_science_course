# Diva Harsoor
# January 29, 2020
# 4625 Political Data Science
# Problem Set 1
# GRADE: 14/15
# Nice clean and well commented code. Mostly missing the sum() step in a couple questions.

#1: I downloaded the csv and hardcoded in the path.
# Other users will have to use their own path.
####GOOD

expenditures<-read.csv("Expends2002.csv", header=T)
exp.df <- as.data.frame(expenditures) #It was already a dataframe btw. you could have checked with class(expenditures)

#2: Renamed
####GOOD

colnames(exp.df)[3] <- "Useless"

#3: Removed columns
####GOOD

exp.df["Useless"] <- NULL
exp.df["Source"] <- NULL

#4: Changed EntType to factor. 8 levels, Levels: c(6, 5, 1, 8, 4, 7, 2, 3)
####GOOD

exp.df <- as.factor(exp.df["EntType"]) #

#5: For number 5, I found a dataset of the 50 states + Washington D.C.
# After looking at the data, I appended the codes for Guam and the U.S. Virgin Islands, 
# but not for American Samoa because there were a couple tagged AS that were meant to be AZ
####GOOD


state_codes <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
                 "HI", "ID", "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
                 "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
                 "SD","TN","TX","UT","VT","VA","WA","WV", "WI", "WY")

state_codes <- c(state_codes, "GU", "VI")

# Then, I changed it to a matrix so I could iterate through it more easily.
# The vector index saves the index of each relevant item
# The vector value saves the state code of each relevant item.
# The vector type holds 0 for missing values and 1 for incorrect values

state.mat <- as.matrix(exp.df["State"])
index <- c()
value <- c()
type <- c()
i <- 0
for (code in state.mat[,"State"]) {
  i <- i + 1
  if (code %in% state_codes == FALSE) {
    index <- c(index, i)
    value <- c(value, code)
    if (code == "  ") {
      type <- c(type, 0)
    }
    else {
      type <- c(type, 1)
    }
  } 
}
# Casting these to integers now so I can use them directly downscript
index <- lapply(index, as.integer)
type <- lapply(type, as.integer)

# 5 part 1 asks us to identify the observations that have non-existent state codes. Here:
nonexistent <- index[type=1]

# Taking a look at the data...I see that GU corresponds to Guam and VI to Virgin Islands.
# AS are meant to be AZ in to cases, but are accurate to American Samoa for the others.
print(paste(index[type==1], value[type==1]))

# Implementing 5 part 2...Change AS to AZ, St to FL (a guess based on Thurman), ZZ to VI, LL to IA
exp.df[7439,11] = "AZ"
exp.df[8994,11] = "FL"
exp.df[11086,11] = "AZ"
exp.df[12333,11] = "VI"
exp.df[12983,11] = "IA"

#6: Removing indices where the state code is missing, reporting before and after numbers
missing.indices <- unlist(index[type == 0])

nrow(exp.df) #20000
exp.df <- exp.df[-missing.indices, ]
nrow(exp.df) #19912

#7: This one didn't go easy into a numeric. My solution turned missing items into NA's
# which I removed from the mean calculation with the na.rm function
####GOOD


my.zip <- as.numeric(levels(exp.df$Zip)) #codes the first two rows wrong
my.zip <- as.numeric(as.character(exp.df$Zip)) #weird but seems to work

# Some values I used to spot check my weird solution: 
# 640, 488, 374, 310, 258, 53, 721 (00000)

# Calculating the mean, which is 48210047. 
####GOOD

mean(my.zip, na.rm=TRUE)

#8-1: Split the string and used length to count. Median is 2.
####GOOD

exp.df$nwords <- sapply(strsplit(as.character(exp.df$Descrip), " "), length)
median(exp.df$nwords)

#8-2: Used substr() and unique(). The number of unique values is 2243.
####GOOD

exp.df$CRPFileridno <- sapply(exp.df$CRPFilerid, substr, start=2, stop=9)
length(unique(exp.df$CRPFileridno))

#8-3: Used substr() and split() to stick all of these in a vector. Then found out
#### Good

exp.df$ZipChars <- sapply(as.character(exp.df$Zip), substr, start=1, stop=4)
names(which(max(table(exp.df$ZipChars))==table(exp.df$ZipChars)))


# that there is no native function to find mode in R. As I am not allowed to make a 
# function (and have been coding for too many hours to think of a better way), 
# I borrowed this following solution from 
# https://www.tutorialspoint.com/r/r_mean_median_mode.htm. The mode was 0.
exp.df$ZipChars <- sapply(as.character(exp.df$Zip), substr, start=1, stop=4)
exp.df$ZipChars <- sapply(as.character(exp.df$ZipChars), strsplit, split=character(0))
zip.vector <- as.integer(unlist(exp.df$ZipChars))

uniqv <- unique(zip.vector)
uniqv[which.max(tabulate(match(zip.vector, uniqv)))]

#8-4: Used ignore.case so all instances of "communications" would be accounted for.
##### -1/2 
#####needed to sum how many occurances of communications: sum(exp.df$Communications)=9

exp.df$Communications <- grepl("communications", exp.df$Descrip, ignore.case = TRUE)

#8-5: I was pretty clunky with this one - I created a columns for each of the 3
# conditions and used the &&, || operators to evaluate them directly in order to
####-1/2 
#### got logical expression correct but I needed you to report the number of true instances: sum(exp.df$FunkyBool) ==19912

# construct the requested variable, which I called FunkyBool. Then, I removed all 
# the extraneous columns.
exp.df$Letter <- sapply(exp.df$CRPFilerid, substr, start=1, stop=1)
exp.df$Test1 <- exp.df$Letter == "N"
exp.df$Test2 <- exp.df$Amount > 500
exp.df$Test3 <- as.character(exp.df$Descrip) == ""
exp.df$FunkyBool <- exp.df$Test1 || exp.df$Test2 && exp.df$Test3
exp.df$FunkyBool

exp.df["ZipChars"] <- NULL
exp.df["Test1"] <- NULL
exp.df["Test2"] <- NULL
exp.df["Test3"] <- NULL

#9: Last one! Used subset(), then used paste0() to construct to filepath (as in #1, it'll
#####GOOD
# be different for other users.)

for (code in state_codes) {
  this.state <- subset(exp.df, State==code)
  path <- paste0("~/Desktop/PDS/", code, ".csv")
  write.csv(this.state, path)
}

