# This is a learning experiment where I try and write a script in R that can handle Counter Reports.
# I'm going to add lots of notes along the way to help the code be readable and talk about the changes I'd like to make.


## READING THE FILES
#### Ideally would read every file from a given folder.
#### Make the files self readable
#### Sort each Counter Report into it's own category.
#### Test the new branch.

#### Using this code for this section - http://www.reed.edu/data-at-reed/resources/R/reading_and_writing.html


## Setting up the folder pathway and creating a file_list variable.
folder <- "C:/Users/ameyer/Desktop/CounterReports"
file_list <- list.files(path=folder, pattern="*.csv")
## Write some conditional test that will validate we are looking at Database Report 1

## This for loop assumes that everything is a DB1 report. Maybe I could sort them first...

## Try to define my own function.
read.counter <-function(x){
  read.csv(x,skip = 7, header=T)
  
}

for (i in 1:length(file_list)){
  assign(file_list[i],
  read.counter(paste(folder, file_list[i], sep='/')))
}


for (i in 1:length(file_list)){
  assign(file_list[i],
  read.csv(paste(folder, file_list[i], sep='/'),skip = 7, header=T))
}




## This is good. All the files are now dataframes in R.
## Need to clean up each of those dataframes, though.

DB1 <- EBSCODB12015.csv
DB2 <- EBSCODB12016.csv

DB1$Reporting.Period.Total <-NULL


for (i in 1:length(file_list)){
  file_list[i]$Reporting.Period.Total <-NULL
}


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
