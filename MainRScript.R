## OVERVIEW AND SET UP
## Right now, this reads all Counter DB1 Reports (R4) from a given folder and creates a dataframe for them.

## Installing packages and loading them
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("xlsx")
library(xlsx)
library(tidyverse)
library(readxl)

## READING THE FILES AND TIDYING THE DATA

## Setting up the folder pathway.
folder <- "C:/Users/ameyer/Desktop/CounterReports"

## Define "Cleaner" functions.
## This function reads from CSV files.
DB1r4_CSV_Cleaner <- function(file){
  x <- read_csv(file, skip=7, col_names = TRUE)
  x <- subset(x, select = -c(5))
  x <- gather(x, Date, Usage, 5:ncol(x))
  x <- separate(x, Date, c("Month", "Year"))
  return(x)
}

## This function reads from Excel files.
DB1r4_xl_Cleaner <- function(file){
  x <- read_excel(file, skip=7, col_names = TRUE)
  x <- subset(x, select = -c(5))
  x <- gather(x, Date, Usage, 5:ncol(x))
  x <- separate(x, Date, c("Month", "Year"))
  return(x)
}

load_csv_data <- function(path) {
  files <- dir(path, pattern ="*.CSV", full.names = TRUE)
  tables <- lapply(files, DB1r4_CSV_Cleaner)
  do.call(rbind, tables)
}

load_xl_data <- function(path) {
  files <- dir(path, pattern ="*.xl*", full.names = TRUE)
  tables <- lapply(files, DB1r4_xl_Cleaner)
  do.call(rbind, tables)
}

Tidy_DB1_data <-unique(rbind(load_csv_data(folder),load_xl_data(folder)))

## Change month abbreviation to number to make sorting easier.
Tidy_DB1_data$Month <- match(tolower(Tidy_DB1_data$Month), tolower(month.abb))

#################################################
##TRANSFORM AND VISUALIZE AND MODEL THE DATA.
#################################################
####General Overview

## See what publishers we have in the dataset.
unique(c(Tidy_DB1_data$Publisher))

## See what platforms we have in the dataset.
unique(c(Tidy_DB1_data$Platform))

## See what databases we have in the dataset.
unique(c(Tidy_DB1_data$Database))

## It would be useful to import pricing data about each database.
## I ant to create a blank dataframe for manual data entry.
## This should help.
## This could be improved with pipes.
Pricing_Info <- unique(subset(Tidy_DB1_data, select=c(Database,Platform,Year)))
Pricing_Info$Price <-"$0"
Pricing_Info <- spread(Pricing_Info, Year, Price, convert=TRUE, fill="$0")
Pricing_Info <- unique(Pricing_Info)  
## Write this to CSV.
write.csv(Pricing_Info, file="C:/Users/ameyer/Desktop/Pricing_Info.csv")
## Expect the user to enter pricing information for each of these resources.

## GRAPH THIS STUFF
ggplot(data=Tidy_DB1_data) + geom_point(mapping = aes(x=Year, y=Usage))


## I'm spreading the data back into a more familiar view. Things more about this.
## Unite Year and Month to make sorting easier.
BasicCounterReport <- unite(Tidy_DB1_data, Date, c(Year,Month), sep="-")
BasicCounterReport <- spread(BasicCounterReport, Date, Usage, convert=TRUE,fill = 0)
BasicCounterReport  <- arrange(BasicCounterReport,Platform)

## Divide data into actual databases and EDS results
## This adds a new column for the total.
BasicCounterReport$Total <- rowSums(BasicCounterReport[5:46])

## Just for fun... write this to Excel.
write.xlsx(BasicCounterReport, "C:/Users/ameyer/Desktop/BasicCounterReport.xlsx",sheetName = "data")
write.xlsx(Tidy_DB1_data, "C:/Users/ameyer/Desktop/TidyReport.xlsx",sheetName = "data")
