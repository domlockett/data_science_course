########################### Political Data Science HW1 ###############################
## Author: Crystal Caragine
## Saved as: PDS_HW1_Caragine
## Date: 1/17/2019
## Class: Political Data Science
#GRADE: 14.5/15
################## #1) Loading data
setwd("C:/Users/cmcar/OneDrive/Documents/School/Stats/Political Data Science")

#clearing enviornment
rm(list=ls())

# loading libraries 
library(tidyverse)
library(dplyr)

#loading data
#Good

DATA <- read.csv("Expends2002.csv")

################## #2) Changing variable name
#Good
DATA <- DATA %>% 
          rename(
            Useless = TransID
                 )

################ #3) Dropping variables
#Good


drops <- c("Useless","Source")
DATA <- DATA[ , !(names(DATA) %in% drops)]

################ #4) Factorizing
#Your factoring shows 8 levels {-1}

DATA$EntTypef <- as.factor(DATA$EntType)
summary(DATA$EntTypef)
# EntTypef has seven levels 

################ #5) Recoding 
#Good


summary(DATA$State)

DATA[which(DATA$State=='LL'),] # observation 12983
DATA[which(DATA$State=='St'),] # 8994
DATA[which(DATA$State=='ZZ'),] # 12333

# Fixing invalid state codes
DATA$State[DATA$State=="LL"] <- "IA"
DATA$State[DATA$State=="St"] <- "FL"
DATA$State[DATA$State=="ZZ"] <- "VA"

summary(DATA$State)
# Incorrect values have been fixed

################ #6) Missing Data
#Good

DATA2 <- DATA[!(DATA$State=="  "), ]

summary(DATA2$State)
# the new data set has 19912 observations, while the original has 20000 observations

################ #7) Making numeric
#Good


DATA2$Zip <- as.numeric(DATA2$Zip)
summary(DATA2$Zip)
#blank/ missing values are ignored 

mean(DATA2$Zip)
# mean=2937.886


############### #8) Creating new variables
#Good


## Number of words in Descrip
DATA2$Descrip <- as.character(DATA2$Descrip)
DATA2$Descripn <- sapply(strsplit(DATA2$Descrip, " "), length)
summary(DATA2$Descripn)
# Median = 2

## Numbers in CRPFilerid 
DATA2$crp.num <- as.numeric(str_extract(DATA2$CRPFilerid, "[0-9]+"))
DATA2$crp.num <- as.factor(DATA2$crp.num)
nlevels(DATA2$crp.num)
# 2243 unique values 

## First four digits of Zip
DATA2$Zip.4 <- substr(DATA2$Zip, 1, 4)
summary(as.factor(DATA2$Zip.4))[1]
# the most common 4 digit zip code is 1257

##  "Communications" in Descrip
#Good


sum(grepl("COMMUNICATIONS", DATA2$Descrip, ignore.case=TRUE))
# 9 true values

## CRPFilerid == N | Amount > 500 & Descrip == !NA
#-1/2 The directions asked you make a variable as well as sum

sum(grepl("N", DATA2$Descrip, ignore.case=TRUE) | ((DATA2$Amount>500) & (!is.na(DATA2$Descrip))))

################# #9) Subsetting by State
#Good


statelist <- by(DATA, DATA$State, function(x)(DATA))

lapply(1:length(statelist), function(i) write.csv(statelist[[i]], 
                                                file = paste0(names(statelist[i]), ".csv"),
                                                row.names = FALSE))

