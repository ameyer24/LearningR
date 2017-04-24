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

##General Overview

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


## This "spreads" the data into a more counter like report.
Summary1 <- Tidy_DB1_data %>%
  filter(Date >= 2013) %>% # Update this filter to create customized date ranges.
  spread(Date, Usage, convert=TRUE,fill = 0) %>%
  arrange(Database,Platform) %>%
  write_csv(paste(export_folder, "Summary1.csv",sep="/"))

## Summarize Usage on the Calendar Year
Summary2 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  group_by(Database, Publisher, User_Activity, Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Year, Total_Usage) %>%
  write_csv(paste(export_folder, "Summary2.csv",sep="/"))

## Summarize Usage on the Academic Year
Summary3 <- Tidy_DB1_data %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Academic_Term = derivedFactor(
    "S1 (Spring)" = (Month==1 | Month==2 | Month==3 | Month==4),
    "S2 (Summer)" = (Month==5 | Month==6 | Month==7 | Month==8),
    "S3 (Fall)" = (Month==9 | Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Acad_Year = paste(Year, Academic_Term, sep=" "))%>%
  group_by(Database, Publisher, User_Activity, Acad_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Acad_Year, Total_Usage) %>%
  write_csv(paste(export_folder, "Summary3.csv",sep="/"))

## Summarize on the Fiscal Year
Summary4 <- Tidy_DB1_data %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year)) %>%
  mutate(Fiscal_Year = paste("FY", Fiscal_Year, sep=" ")) %>%
  group_by(Database, Publisher, User_Activity, Fiscal_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Fiscal_Year, Total_Usage) %>%
  write_csv(paste(export_folder, "Summary4.csv",sep="/"))

















#############################
## IMPORT PRICING INFORMATION
## This creates a blank template to help with the import of pricing data.
DB_Pricing_Blank <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher,Platform, Year) %>%
  mutate(Price ="") %>%
  mutate(Notes = "") %>%
  mutate(Category="") %>%
  mutate(Ordering_Site = "") %>%
  mutate(Ordering_Cycle = "") %>%
  spread(Year,Price) %>%
  write_csv(paste(export_folder, "DB_Pricing_Blank.csv",sep="/"))

## Imports the pricing information file.
Raw_Database_Pricing <- read_csv(paste(export_folder, "DB_Pricing.csv",sep="/"), col_names = TRUE)

## Creating some variables to describe the size and shape of the pricing data.
## This sets the number of descriptive columns at 7 (the rest are years)
DB_Pricing_Description <- 7
DB_Pricing_Years <- ncol(Raw_Database_Pricing)-DB_Pricing_Description

## Creates tidy dataframe of just database pricing.
## Does not use the rest of the descriptive information.
Tidy_Database_Pricing <- Raw_Database_Pricing %>%
  gather(Fiscal_Year, Cost, (DB_Pricing_Description +1):(ncol(Raw_Database_Pricing))) %>%
  mutate(Cost = as.numeric(Cost)) %>%
  subset(select = -c(4:7))

###############################
## Look at pricing information.
## Create a simple table of pricing information.
Cost1 <- Tidy_Database_Pricing %>%
  spread(Fiscal_Year,Cost)

## Adds a Total_Cost field with the sum of the cost.
Cost1$Total_Cost<- rowSums(Cost1[4:8], na.rm=TRUE)

## Filters our databases without pricing and arranges in order from $$$$ to $.
Cost2 <- Cost1 %>%
  filter(Total_Cost > 0) %>%
  arrange(desc(Total_Cost))

## Calculate the average cost increase over time.

## Here is a simple graph of pricing for a given database.
CostGraph1 <- Tidy_Database_Pricing %>%
  filter(Database=="CINAHL Complete") %>%
  ggplot(aes(x=Fiscal_Year,y=Cost)) + geom_bar(stat="identity")
CostGraph1



















































#############################
##Cost per use work

## Compare the fiscal year usage to fiscal year cost for a particular database.
## Combine data into one dataframe.

FY_Usage <- Tidy_DB1_data %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year)) %>%
  group_by(Database, Publisher, User_Activity, Fiscal_Year) %>%
  summarize(Measure = sum(Usage))

FY_Cost <- Raw_Database_Pricing %>%
  gather(Fiscal_Year, Cost, (DB_Pricing_Description +1):(ncol(Raw_Database_Pricing))) %>%
  mutate(Cost = as.numeric(Cost), Fiscal_Year = as.numeric(Fiscal_Year)) %>%
  subset(select = -c(3:7)) %>%
  mutate(Measurement = "Cost")

class(FY_Usage$Fiscal_Year)


















#############################
## GRAPHING USAGE

## Filter by database
Graph1 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  ggplot() +
  geom_line(mapping = aes(x=Date, y=Usage, color=User_Activity)) +
  scale_x_yearmon()
Graph1

## Filter by database and User Activity
Graph2 <- Tidy_DB1_data %>%
  filter(User_Activity=="Record Views") %>%
  filter(Database=="CINAHL Complete") %>%
  ggplot() +
  geom_line(mapping = aes(x=Date, y=Usage, color=Database)) +
  scale_x_yearmon()
Graph2

## Filtered by Database and Summed by Year.
Graph1_1 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  ## sum by year?
  ggplot() +
  geom_line(mapping = aes(x=Date, y=Usage, color=User_Activity)) +
  scale_x_yearmon()
Graph1_1



## Uses the "mutated" data frame to academic term analysis

Graph3Data <- Mutated1 %>%
  filter(Database=="Academic Search Complete") %>%
  filter(Year>2013) %>%
  mutate(Academic_Season = paste(Year, Academic_Term, sep="-")) %>%
  group_by(Database, Publisher, Platform, User_Activity, Academic_Season)%>%
  summarize(Usage=sum(Usage))

Graph3 <- Graph3Data %>%
  ggplot(aes(x=Academic_Season,y=Usage, fill=User_Activity)) +
  geom_bar(stat="identity")
Graph3

Graph3_1 <- Mutated1 %>%
  filter(Database=="Academic Search Complete") %>%
  filter(Year>2014) %>%
  mutate(Academic_Season = paste(Year, Academic_Term, sep="-")) %>%
  group_by(Database, Publisher, Platform, User_Activity, Academic_Season)%>%
  summarize(Usage=sum(Usage)) %>%
  ggplot(aes(x=Academic_Season,y=Usage, fill=User_Activity)) +
  geom_bar(stat="identity")
Graph3_1

Graph4 <- Mutated1 %>%
  filter(Database=="Academic Search Complete") %>%
  filter(Year>2013) %>%
  mutate(Academic_Season = paste(Year, Academic_Term, sep="-")) %>%
  group_by(Database, Publisher, Platform, User_Activity, Academic_Season)%>%
  summarize(Usage=sum(Usage)) %>%
  ggplot(aes(x=Academic_Season,y=Usage)) +
  geom_point(aes(color=User_Activity))
Graph4

## End Graphing Section

###############################
##Exporting
## I'm spreading the data back into a more familiar view.
## Unite Year and Month to make sorting easier.

BasicCounterReport <- Tidy_DB1_data %>%
  spread(Date, Usage, convert=TRUE,fill = 0) %>%
  arrange(Database,Platform)

## This adds a new column for the total.
BasicCounterReport$Total <- rowSums(BasicCounterReport[5:46])

## Just for fun... write this to Excel.
write.xlsx(BasicCounterReport, "C:/Users/ameyer/Desktop/BasicCounterReport.xlsx",sheetName = "data")
write.xlsx(Tidy_DB1_data, "C:/Users/ameyer/Desktop/TidyReport.xlsx",sheetName = "data")
