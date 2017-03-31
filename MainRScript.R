##OVERVIEW
## Right now, this reads all Counter DB1 Reports (R4) from a given folder and creates a dataframe for them.

## Installing packages and loading them
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("xlsx")
library(xlsx)
library(tidyverse)
library(readxl)

## READING THE FILES
## This reads every CSV or Excel file from a given folder.
## Source for this section - http://www.reed.edu/data-at-reed/resources/R/reading_and_writing.html

## Setting up the folder pathway and creating the file_list variables.
folder <- "C:/Users/ameyer/Desktop/CounterReports"

## This sorts the files into different lists based on whether they are CSV or Excel files.
file_list_csv <- list.files(path=folder, pattern="*.CSV")
file_list_xl <- list.files(path=folder, pattern="*.xl*")

## Counting up the total number of files...
number_of_files <- length(file_list_csv) + length(file_list_xl)

## This iterates through the list of CSV files and creates dataframes for each file
for (i in 1:length(file_list_csv)){
  assign(paste0("CounterReport",i), read_csv(paste(folder, file_list_csv[i], sep='/'),skip=7))
}

## This iterates through list of Excel files and creates dataframes for each file
## Updated naming conventions to prevent overriding existing dataframes.
for (i in 1:length(file_list_xl)){
  assign(paste0("CounterReport",i+(length(file_list_csv))), read_excel(paste(folder, file_list_xl[i], sep='/'),skip=7))
}

## Definign my own function.
#### Remove the "Reporting Period Total" column. Because we don't need it.
#### Next, gather the data together to make one long DF that seperates each variable.
#### Finally, seperate the data into Month and Year.
#### Return the cleaned dataframe.

CounterCleaner <- function(x){
  x <- subset(x, select = -c(5))
  x <- gather(x, date, usage, 5:ncol(x))
  x <- separate(x, date, c("Month", "Year"))
  return(x)
}

## I know this is a mess. Do this another way?

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
CounterReport13 <- CounterCleaner(CounterReport13)
CounterReport14 <- CounterCleaner(CounterReport14)
CounterReport15 <- CounterCleaner(CounterReport15)

MasterCounterReport <- rbind(CounterReport1, CounterReport2, CounterReport3,CounterReport4,CounterReport5, CounterReport6, CounterReport7, CounterReport8, CounterReport9, CounterReport10, CounterReport11, CounterReport12, CounterReport13, CounterReport14, CounterReport15)

## Mess over.

## Deduplicate the Master Counter Report.
MasterCounterReport1 <- unique(MasterCounterReport)

## Change month abbreviation to number to make sorting easier.
MasterCounterReport1$Month <- match(tolower(MasterCounterReport1$Month), tolower(month.abb))

## United Year and then Month to make sorting easier.
BasicCounterReport <- unite(MasterCounterReport1, Date, c(Year,Month), sep="-")
BasicCounterReport <- spread(BasicCounterReport, Date, usage, convert=TRUE)
BasicCounterReport  <- arrange(BasicCounterReport,Platform)


#### Exploring the Data
## See what publishers we have in the dataset.
unique(c(BasicCounterReport$Publisher))

## See what platforms we have in the dataset.
unique(c(BasicCounterReport$Platform))

## See what databases we have in the dataset.
unique(c(BasicCounterReport$Database))

## Just for fun... write this to Excel.

# write.xlsx(MasterCounterReport, "C:/Users/ameyer/Desktop/CounterReports/MasterCounterReport.xlsx",sheetName = "data")
# 
write.xlsx(BasicCounterReport, "C:/Users/ameyer/Desktop/CounterReports/BasicCounterReport.xlsx",sheetName = "data")
