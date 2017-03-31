## This is a learning experiment where I try and write a script in R that can handle Counter Reports.
## I'm going to add lots of notes along the way to help the code be readable and talk about the changes I'd like to make.
## SETTING THINGS UP
#install.packages("tidyverse")
#install.packages("readxl")
library(tidyverse)
library(readxl)

## READING THE FILES
#### This reads every CSV or Excel file from a given folder. Source for this section - http://www.reed.edu/data-at-reed/resources/R/reading_and_writing.html

## Setting up the folder pathway and creating the file_list variables.
folder <- "C:/Users/ameyer/Desktop/CounterReports"

## This sorts the Counter Reports into CSV or XL file formats.
file_list_csv <- list.files(path=folder, pattern="*.csv")
file_list_xl <- list.files(path=folder, pattern="*.xl*")

## Counting up the total number of files...
number_of_files <- length(file_list_csv) + length(file_list_xl)


## This iterates through the list of CSV filese and creates dataframes for each report
for (i in 1:length(file_list_csv)){
  assign(paste0("CounterReport",i), read_csv(paste(folder, file_list_csv[i], sep='/'),skip=7))
}

## This iterates through list of Excel files and creates dataframes for each report.
## Updated naming conventions to prevent overriding existing dataframes.
for (i in 1:length(file_list_xl)){
  assign(paste0("CounterReport",i+(length(file_list_csv))), read_excel(paste(folder, file_list_xl[i], sep='/'),skip=7))
}

### Definign my own function.

CounterCleaner <- function(x){
  x <- subset(x, select = -c(5))
  x <- gather(x, date, usage, 5:ncol(x))
  x <- separate(x, date, c("Month", "Year"))
  return(x)
}

CounterReport1 <- CounterCleaner(CounterReport1)
CounterReport2 <- CounterCleaner(CounterReport2)
CounterReport3 <- CounterCleaner(CounterReport3)
CounterReport4 <- CounterCleaner(CounterReport4)
CounterReport5 <- CounterCleaner(CounterReport5)
CounterReport6 <- CounterCleaner(CounterReport6)
CounterReport7 <- CounterCleaner(CounterReport7)
CounterReport8 <- CounterCleaner(CounterReport8)
CounterReport9 <- CounterCleaner(CounterReport9)
CounterReport10 <- CounterCleaner(CounterReport10)
CounterReport11 <- CounterCleaner(CounterReport11)
CounterReport12 <- CounterCleaner(CounterReport12)



## Remove the "Reporting Period Total" column. Because we don't need it.
## This is stupid. Remove by column name instead. This is dangerous!

for (i in 1:10){
  CounterReport[i] <- 1
}
  #CounterReport[i] <- subset(CounterReport[i], select = -c(5))


## Loop this somehow.

CounterReport1 <- subset(CounterReport1, select = -c(5))
CounterReport2 <- subset(CounterReport2, select = -c(5))
CounterReport3 <- subset(CounterReport3, select = -c(5))
CounterReport4 <- subset(CounterReport4, select = -c(5))
CounterReport5 <- subset(CounterReport5, select = -c(5))
CounterReport6 <- subset(CounterReport6, select = -c(5))
CounterReport7 <- subset(CounterReport7, select = -c(5))
CounterReport8 <- subset(CounterReport8, select = -c(5))
CounterReport9 <- subset(CounterReport9, select = -c(5))
CounterReport10 <- subset(CounterReport10, select = -c(5))

## This command gathers together the data to make one long DF that seperates each variable.
CounterReport1 <- gather(CounterReport1.DF, date, usage, 5:ncol(CounterReport1.DF))
CounterReport2.DF <- gather(CounterReport2.DF, date, usage, 5:ncol(CounterReport2.DF))
CounterReport3.DF <- gather(CounterReport3.DF, date, usage, 5:ncol(CounterReport3.DF))
CounterReport4.DF <- gather(CounterReport4.DF, date, usage, 5:ncol(CounterReport4.DF))
CounterReport5.DF <- gather(CounterReport5.DF, date, usage, 5:ncol(CounterReport5.DF))
CounterReport6.DF <- gather(CounterReport6.DF, date, usage, 5:ncol(CounterReport6.DF))
CounterReport7.DF <- gather(CounterReport7.DF, date, usage, 5:ncol(CounterReport7.DF))
CounterReport8.DF <- gather(CounterReport8.DF, date, usage, 5:ncol(CounterReport8.DF))
CounterReport9.DF <- gather(CounterReport9.DF, date, usage, 5:ncol(CounterReport9.DF))
CounterReport10.DF <- gather(CounterReport10.DF, date, usage, 5:ncol(CounterReport10.DF))

## This command seperates out the data in month and year.
CounterReport1.DF <- separate(CounterReport1.DF, date, c("Month", "Year"))
CounterReport2.DF <- separate(CounterReport2.DF, date, c("Month", "Year"))
CounterReport3.DF <- separate(CounterReport3.DF, date, c("Month", "Year"))
CounterReport4.DF <- separate(CounterReport4.DF, date, c("Month", "Year"))
CounterReport5.DF <- separate(CounterReport5.DF, date, c("Month", "Year"))
CounterReport6.DF <- separate(CounterReport6.DF, date, c("Month", "Year"))
CounterReport7.DF <- separate(CounterReport7.DF, date, c("Month", "Year"))
CounterReport8.DF <- separate(CounterReport8.DF, date, c("Month", "Year"))
CounterReport9.DF <- separate(CounterReport9.DF, date, c("Month", "Year"))
CounterReport10.DF <- separate(CounterReport10.DF, date, c("Month", "Year"))

## All of those dataframes look great!
## Merge them into one main dataframe!

MasterCounterReport <- rbind(CounterReport1.DF, CounterReport2.DF, CounterReport3.DF,CounterReport4.DF,CounterReport5.DF, CounterReport6.DF, CounterReport7.DF, CounterReport8.DF, CounterReport9.DF, CounterReport10.DF)

## And I think this is tidy dataset!
## Just for fun, I can "unite" and spread" this data back into something like the original form.
BasicCounterReport <- unite(MasterCounterReport, date, c(Month,Year), sep=".")
BasicCounterReport <- spread(BasicCounterReport, date, usage)


