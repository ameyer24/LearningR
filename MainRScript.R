## OVERVIEW AND SET UP

## Install packages.
install.packages("tidyverse")
install.packages("readxl")
install.packages("xlsx")
install.packages("zoo")
install.packages("mosaic")
## Load Packages.
library(mosaic)
library(xlsx)
library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)

## READING THE FILES AND TIDYING THE DATA
## Setting up the folder pathway.
folder <- "C:/Users/ameyer/Desktop/CounterReports"
export_folder <- "C:/Users/ameyer/Desktop/CounterReportsReports"

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

## Mutate data to include additional date grouping options.
## Not sure if this will be helpful. But it might be!
## This data is still "tidy" but with additional date information added.
Mutated1 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Fiscal_Quarter = derivedFactor(
    "Q1" = (Month==1 | Month==2 | Month==3),
    "Q2" = (Month==4 | Month==5 | Month==6),
    "Q3" = (Month==7 | Month==8 | Month==9),
    "Q4" = (Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Academic_Term = derivedFactor(
    "Spring" = (Month==1 | Month==2 | Month==3 | Month==4),
    "Summer" = (Month==5 | Month==6 | Month==7 | Month==8),
    "Fall" = (Month==9 | Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year))
  
## Mutate to combine "similar" databases and/or upgrades.
## I'm going to do this all manually.
## I'm going to try all the existing data and just add to it.
## These nested ifelses are nasty... is there a better way!!?
Mutated2 <- Tidy_DB1_data %>%
  mutate(Database_Name = 
           ifelse((Database == "Academic Search Complete"|Database == "Academic Search Elite"),
                  "Academic Search Family",
                  ifelse((Database == "CINAHL Complete" |Database == "CINAHL"),
                         "CINAHL Family",
                         ifelse((Database == "MEDLINE Complete" |Database == "MEDLINE"),
                                "Medline Family",
                                Database)
                         )
                  )
         )

           



## Summmarize total usage. Arrange in descending order by database.
Summary1 <- Tidy_DB1_data %>%
  group_by(Database, User_Activity) %>%
  summarize(total = sum(Usage)) %>%
  arrange(desc(total))


## Summarize Usage on the Calendar Year
Summary2 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  group_by(Database, Publisher, User_Activity, Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Year, Total_Usage) %>%
  write_csv(paste(export_folder, "Summary2.csv",sep="/"))

## Summarize on the Academic Year
## Takes the "mutated" data frame created earlier.
## Would it be better to embed that stuff here?
## Try it both ways? Seems like there is an upper limit to chaining functions.

Summary3_1 <- Mutated1 %>%
  mutate(Acad_Year = paste(Academic_Term, Year, sep="-"))%>%
  group_by(Database, Publisher, User_Activity, Acad_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Acad_Year, Total_Usage) %>%
  write_csv(paste(export_folder, "Summary3_1.csv",sep="/"))

## This goes directly from the Tidy Data Frame.
Summary3_2 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Academic_Term = derivedFactor(
    "Spring" = (Month==1 | Month==2 | Month==3 | Month==4),
    "Summer" = (Month==5 | Month==6 | Month==7 | Month==8),
    "Fall" = (Month==9 | Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Acad_Year = paste(Year, Academic_Term, sep="-"))%>%
  group_by(Database, Publisher, User_Activity, Acad_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Acad_Year, Total_Usage)

## Summarize on the Fiscal Year
## Deciding to skip the extra step and calculate just from month and year.
Summary4_1 <- Mutated1 %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year))%>%
  group_by(Database, Publisher, User_Activity, Fiscal_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Fiscal_Year, Total_Usage)

## This goes directly from the tidy dataframe.
Summary4_2 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year))%>%
  group_by(Database, Publisher, User_Activity, Fiscal_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Fiscal_Year, Total_Usage)

## If the sum "regular searches" for a database is less than 10 - classify as "EDS_Search.
Summary5 <- Tidy_DB1_data %>%
  group_by(Database, Publisher, User_Activity) %>%
  filter(User_Activity =="Record Views")%>%
  summarize(Sum_Record_Views= sum(Usage)) %>%
  mutate(DB_Value = ifelse(Sum_Record_Views >10, "Core_Database","EDS_Search")) %>%
  arrange(desc(Sum_Record_Views))









#############################
## IMPORT PRICING INFORMATION

## rename year columns!!
## Add publisher column (to make tidier)
DB_Pricing_Blank <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher, Year) %>%
  mutate(Price ="") %>%
  spread(Year,Price) %>%
  mutate(Notes = "") %>%
  write_csv(paste(export_folder, "DB_Pricing_Blank.csv",sep="/"))

## Merging Summary 5 and DB_Pricing Blank

DB_Pricing_Blank1 <- DB_Pricing_Blank %>%
  merge(Summary5,by=c("Database","Publisher")) %>%
  select(-User_Activity) %>%
  select(-Sum_Record_Views) %>%
  filter(DB_Value =="Core_Database")%>%
  write_csv(paste(export_folder, "DB_Pricing_Blank1.csv",sep="/"))

## Imports the pricing information file.
##
Database_Pricing <- read_csv(paste(export_folder, "DB_Pricing.csv",sep="/"), col_names = TRUE)

## Create "tidy" database pricing.
## An attempt to add this information to the tidy db information.

Tidy_Database_Pricing <- Database_Pricing %>%
  filter(Database =="Academic Search Complete") %>%
  mutate(Platform = "test") %>%
  gather(Fiscal_Year, Cost, 3:6) # Need to update this to make it work all the time.

  

#############################
##Cost per use work
## Start small - combine SUmmary2 and Cost per use
## Summary2 is annual usage. Cost is on an annual basis.

## I might be possible to add 1/12 of the cost to the original tidy data...
## Is that a good idea or not?
## Maybe add it not as a new column but as a new row?!!

Cost_Per_Use1 <- Summary2 %>%
  left_join(Database_Pricing, by=c("Database","Publisher"))

## It would be helpful to rename the columns.
## I should do that earlier. Revise earlier stuff.
## I manually renamed the columns in the blank pricing thing...revise that code!

Cost_Per_Use2 <- Cost_Per_Use1 %>%
  filter(Database=="Academic Search Complete") %>%
  mutate(cost_per_2015 = `2015_Cost`/`2015`)



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
