# This is a learning experiment where I try and write a script in R that can handle Counter Reports.
# I'm going to add lots of notes along the way to help the code be readable and talk about the changes I'd like to make.


## READING THE FILES
#### Ideally would read every file from a given folder.
#### Make the files self readable
#### Sort each Counter Report into it's own category.
## Test the new branch.

IngestCounterDB1.DF <- read.csv("C:/Users/ameyer/Desktop/EBSCODB12016.csv",
                          skip = 7, # skip the introductions
                          header = T) # this data has headers


str(IngestCounterDB1.DF) # this provides an overview of the structure for this dataframe. Looks good!

## CLEANING AND STRUCTURING
## Tidying up dataframe.
## I'm using the r package tidyr for this
## This loads that package
library(tidyr)

## Don't need the "Reporting Period Total because that doesn't matter.
IngestCounterDB1.DF$Reporting.Period.Total <-NULL
str(IngestCounterDB1.DF) # confirms that the column has been deleted.


## This command gathers together the data to make one long DF that seperates each variable.
CounterDB.DF.Gathered <- gather(IngestCounterDB1.DF, date, usage, Jan.2016:Dec.2016)
str(CounterDB.DF.Gathered)
## Need to update/improve that command to accept any data - that is, don't specify the column headings.
##Seperate the date field into year and month
CounterDB.DF.GatheredSep <- separate(CounterDB.DF.Gathered, date, c("Month", "Year"))

## I think this new DF looks great.
## I could imagine adding on data from other counter CSV files to make a "master dataframe"
## and then de-duplicating.

## Just for fun, I can "unite" and spread" this data back into something like the original form.
CounterDB.DF.GatheredUnited <-unite(CounterDB.DF.GatheredSep, date, c(Month, Year),sep=".")
CounterDB.DF.Standard <- spread(CounterDB.DF.GatheredUnited, date, usage)
## Need to re-order columns by month. Otherwise great!
