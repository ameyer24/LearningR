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
  x <- gather(x, date, usage, 5:ncol(x))
  x <- separate(x, date, c("Month", "Year"))
  return(x)
}

## This function reads from Excel files.
DB1r4_xl_Cleaner <- function(file){
  x <- read_excel(file, skip=7, col_names = TRUE)
  x <- subset(x, select = -c(5))
  x <- gather(x, date, usage, 5:ncol(x))
  x <- separate(x, date, c("Month", "Year"))
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

All_data <- rbind(load_csv_data(folder),load_xl_data(folder))
Tidy_DB1_data <- unique(All_data)

## Change month abbreviation to number to make sorting easier.
Tidy_DB1_data$Month <- match(tolower(Tidy_DB1_data$Month), tolower(month.abb))

##TRANSFORM AND VISUALIZE AND MODEL THE DATA.

## I'm spreading the data back into a more familiar view. Things more about this.
## Unite Year and Month to make sorting easier.
BasicCounterReport <- unite(Tidy_DB1_data, Date, c(Year,Month), sep="-")
BasicCounterReport <- spread(BasicCounterReport, Date, usage, convert=TRUE)
BasicCounterReport  <- arrange(BasicCounterReport,Platform)

## See what publishers we have in the dataset.
unique(c(BasicCounterReport$Publisher))

## See what platforms we have in the dataset.
unique(c(BasicCounterReport$Platform))

## See what databases we have in the dataset.
unique(c(BasicCounterReport$Database))

## Divide data into actual databases and EDS results
## This adds a new column for the total.
BasicCounterReport$Total <- rowSums(BasicCounterReport[5:46])

## Just for fun... write this to Excel.

write.xlsx(BasicCounterReport, "C:/Users/ameyer/Desktop/CounterReports/BasicCounterReport.xlsx",sheetName = "data")
