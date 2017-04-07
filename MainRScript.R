## OVERVIEW AND SET UP
## Right now, this reads all Counter DB1 Reports (R4) from a given folder and creates a dataframe for them.

## Installing packages and loading them
install.packages("tidyverse")
install.packages("readxl")
install.packages("xlsx")
install.packages("zoo")
library(xlsx)
library(tidyverse)
library(readxl)
library(zoo)

## READING THE FILES AND TIDYING THE DATA

## Setting up the folder pathway.
folder <- "C:/Users/ameyer/Desktop/CounterReports"

## Define "Cleaner" functions.
## This function reads from CSV files.
DB1r4_CSV_Cleaner <- function(file){
  require(zoo)
  x <- read_csv(file, skip=7, col_names = TRUE)
  x <- subset(x, select = -c(5))
  x <- gather(x, Date, Usage, 5:ncol(x))
  x$Date <- as.yearmon(x$Date, "%b-%Y")
  return(x)
}

## This function reads from Excel files.
DB1r4_xl_Cleaner <- function(file){
  require(zoo)
  x <- read_excel(file, skip=7, col_names = TRUE)
  x <- subset(x, select = -c(5))
  x <- gather(x, Date, Usage, 5:ncol(x))
  x$Date <- as.yearmon(x$Date, "%b-%Y")
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
## Do a little more tidying.
## Remove space from the column name.
Tidy_DB1_data <- plyr::rename(Tidy_DB1_data, replace = c("User Activity" = "User_Activity"))
## Convert Usage to a number.
Tidy_DB1_data$Usage <- as.numeric(Tidy_DB1_data$Usage)
class(Tidy_DB1_data$Date)



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

summary_TD <- Tidy_DB1_data %>%
  group_by(Database, User_Activity) %>%
  summarize(total = sum(Usage)) %>%
  arrange(desc(total))

## Summarize Usage on the Calendar Year
Summary1 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  group_by(Database, Publisher, User_Activity, Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Year, Total_Usage) %>%
  write_csv("C:/Users/ameyer/Desktop/Summary1.csv")
install.packages("mosaic")
library(mosaic)

## Summarize on the Fiscal Year
## group into fiscal years (or academic terms?)
Summary2 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(FY = derivedFactor(
    "Q1" = (Month==1 | Month==2 | Month==3),
    "Q2" = (Month==4 | Month==5 | Month==6),
    "Q3" = (Month==7 | Month==8 | Month==9),
    "Q4" = (Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  group_by(Database, Publisher, User_Activity, FY) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(FY, Total_Usage)

## Mutate data to include additional year groupings.
## Not sure if this will be helpful. But it might be!
Mutated1 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(FQ = derivedFactor(
    "Q1" = (Month==1 | Month==2 | Month==3),
    "Q2" = (Month==4 | Month==5 | Month==6),
    "Q3" = (Month==7 | Month==8 | Month==9),
    "Q4" = (Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Acad_Term = derivedFactor(
    "Spring" = (Month==1 | Month==2 | Month==3 | Month==4),
    "Summer" = (Month==5 | Month==6 | Month==7 | Month==8),
    "Fall" = (Month==9 | Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  ))



#############################
## PRICING INFORMATION

DB_Pricing_Blank <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher, Year) %>%
  mutate(Price ="") %>%
  spread(Year,Price) %>%
  mutate(Notes = "") %>%
  write_csv("C:/Users/ameyer/Desktop/DB_Pricing_Blank.csv")
  

## Imports the pricing information file.
##
Database_Pricing <- read_csv("C:/Users/ameyer/Desktop/DB_Pricing.csv", col_names = TRUE)

#############################
##Cost per use work
## Starting with just Business Source Complete
ROI1 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  mutate(Year = year(Date)) %>%
  group_by(User_Activity, Year) %>%
  summarize(Total = sum(Usage)) %>%
  arrange(desc(Year))


#############################
## GRAPH THIS STUFF
## Way too much data. Got to start small.
## Limit to JSTOR data
## Plots JSTOR data and user activity
Tidy_DB1_data %>%
  filter(Database=="Books at JSTOR") %>%
  ggplot()+ geom_line(mapping = aes(x=Date, y=Usage, color=User_Activity))+ scale_x_yearmon()

## plots just record views for JSTOR
Tidy_DB1_data %>%
  filter(User_Activity=="Record Views" & Database=="JSTOR") %>%
  ggplot()+ geom_line(mapping = aes(x=Date, y=Usage, color=Database))+ scale_x_yearmon()


## End Grpahing Section

###############################
##Exporting
## I'm spreading the data back into a more familiar view. Things more about this.
## Unite Year and Month to make sorting easier.

BasicCounterReport <- Tidy_DB1_data %>%
  spread(Date, Usage, convert=TRUE,fill = 0) %>%
  arrange(Database,Platform)

## Divide data into actual databases and EDS results
## This adds a new column for the total.
BasicCounterReport$Total <- rowSums(BasicCounterReport[5:46])

## Just for fun... write this to Excel.
write.xlsx(BasicCounterReport, "C:/Users/ameyer/Desktop/BasicCounterReport.xlsx",sheetName = "data")
write.xlsx(Tidy_DB1_data, "C:/Users/ameyer/Desktop/TidyReport.xlsx",sheetName = "data")
