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
    "S1 (Spring)" = (Month==1 | Month==2 | Month==3 | Month==4),
    "S2 (Summer)" = (Month==5 | Month==6 | Month==7 | Month==8),
    "S3 (Fall)" = (Month==9 | Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year))

  
## Mutate to combine "similar" databases and/or upgrades.
## I'm going to do this all manually.
## I'm going to try all the existing data and just add to it.
# ## These nested ifelses are nasty... is there a better way!!?
# Mutated2 <- Tidy_DB1_data %>%
#   mutate(Database_Name = 
#            ifelse((Database == "Academic Search Complete"|Database == "Academic Search Elite"),
#                   "Academic Search Family",
#                   ifelse((Database == "CINAHL Complete" |Database == "CINAHL"),
#                          "CINAHL Family",
#                          ifelse((Database == "MEDLINE Complete" |Database == "MEDLINE"),
#                                 "Medline Family",
#                                 Database)
#                          )
#                   )
#          )
# 
#            






## This "spreads" the data into a more counter like report.
Summary1 <- Tidy_DB1_data %>%
  filter(Date >= 2013) %>% # Update this filter to create customized date ranges.
  spread(Date, Usage, convert=TRUE) %>%
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

## What is the ratio between different measures?
Summary5 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  filter(Database == "Academic Search Complete") %>%
  group_by(Database, Publisher, Year, User_Activity) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  # mutate(change = Total_Usage/lag(Total_Usage)) %>%
  spread(User_Activity, Total_Usage) %>%
  mutate(Viewed_Clicked = Record Views) %>%
  write_csv(paste(export_folder, "Summary5.csv",sep="/"))








#############################
## IMPORT PRICING INFORMATION
## This creates a blank template to help with the import of pricing data.
DB_Pricing_Blank <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher,Platform, Year) %>%
  mutate(Price ="", Notes="", Fund="", Ordering_Site="", Ordering_Cycle="Fiscal Year") %>%
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














































class(Tidy_Database_Pricing$Cost)


Database_Pricing2 <- read_csv(paste(export_folder, "DB_Pricing.csv",sep="/"), col_names = TRUE)
  filter(Database =="Academic Search Complete") %>%
  gather(Fiscal_Year, Cost, Num_of_years:(Num_of_years + 3))%>%
  mutate(Cost = as.numeric(Cost))

# ## Trying to convert the yearly pricing information into monthly...
# Tidy_Database_Pricing1 <-   Database_Pricing %>%
#   filter(Database =="Academic Search Complete") %>%
#   gather(Fiscal_Year, Cost, Num_of_years:(Num_of_years + 3)) %>%
#   mutate(Monthly_Cost = as.numeric(Cost)/12) %>%








#############################
##Cost per use work

## Compare the fiscal year usage to fiscal year cost for a particular database.
## Combine data into one dataframe.

FY_Usage <- Tidy_DB1_data %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year)) %>%
  group_by(Database, Publisher, User_Activity, Fiscal_Year) %>%
  summarize(Measure = sum(Usage)) %>% 
  plyr::rename(replace = c("User_Activity" = "Measurement"))

FY_Cost <- Raw_Database_Pricing %>%
  gather(Fiscal_Year, Cost, (DB_Pricing_Description +1):(ncol(Raw_Database_Pricing))) %>%
  mutate(Measure = as.numeric(Cost), Fiscal_Year = as.numeric(Fiscal_Year)) %>%
  subset(select = -c(3:7)) %>%
  subset(select = -c(4)) %>%
  mutate(Measurement = "Cost")

## I've forced the Usage and Cost into dataframes with similar shapes and columns.
## Now I combine them on rows

FY_Overview <- do.call("rbind",list((as.data.frame(FY_Usage)), (as.data.frame(FY_Cost))))

## Testing
FY_Overview_Test <- FY_Overview %>%
  filter(Database=="Business Source Complete") %>%
  spread(Fiscal_Year, Measure)














#############################
## GRAPHING USAGE

## Filter by database
Graph1 <- Tidy_DB1_data %>%
  filter(Database=="Chicago Manual of Style Online") %>%
  ggplot() +
  geom_line(mapping = aes(x=Date, y=Usage, color=User_Activity)) +
  scale_x_yearmon()
Graph1

## Filter by database and User Activity
Graph2 <- Tidy_DB1_data %>%
  filter(User_Activity=="Record Views") %>%
  filter(Database=="Business Source Complete") %>%
  ggplot(aes(Date, Usage)) +
  geom_line(mapping = aes(color=Database)) +
  geom_smooth(span=0.7) +
  scale_x_yearmon()
Graph2

## Filtered by Database and Summed by Year.
Graph3 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  mutate(Year=year(Date)) %>%
  group_by(Database, Publisher, Platform, User_Activity, Year)%>%
  summarize(Usage=sum(Usage)) %>%
  ggplot() +
  geom_line(mapping = aes(x=Year, y=Usage, color=User_Activity))
Graph3

## Filtered by database and year and then summarized by academic term.
Graph4 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  filter(Year>2013) %>%
  mutate(Academic_Term = derivedFactor(
    "S1 (Spring)" = (Month==1 | Month==2 | Month==3 | Month==4),
    "S2 (Summer)" = (Month==5 | Month==6 | Month==7 | Month==8),
    "S3 (Fall)" = (Month==9 | Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Academic_Year = paste(Year, Academic_Term, sep=" "))%>%
  group_by(Database, Publisher, Platform, User_Activity, Academic_Year) %>%
  summarize(Usage=sum(Usage)) %>%
  ggplot(aes(Academic_Year,Usage)) +
  geom_line(aes(color=User_Activity, group=User_Activity))
Graph4

## Same as Graph4 but User_Activity is faceted.
Graph5 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  filter(Year>2013) %>%
  mutate(Academic_Term = derivedFactor(
    "S1 (Spring)" = (Month==1 | Month==2 | Month==3 | Month==4),
    "S2 (Summer)" = (Month==5 | Month==6 | Month==7 | Month==8),
    "S3 (Fall)" = (Month==9 | Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Academic_Year = paste(Year, Academic_Term, sep=" "))%>%
  group_by(Database, Publisher, Platform, User_Activity, Academic_Year) %>%
  summarize(Usage=sum(Usage)) %>%
  ggplot(aes(Academic_Year,Usage)) +
  facet_grid(. ~ User_Activity) + 
  geom_line(aes(group=User_Activity))
Graph5

## Bar Graph grouped by seasons and then year.
Graph6 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  filter(Year>2013) %>%
  mutate(Academic_Term = derivedFactor(
    "S1 (Spring)" = (Month==1 | Month==2 | Month==3 | Month==4),
    "S2 (Summer)" = (Month==5 | Month==6 | Month==7 | Month==8),
    "S3 (Fall)" = (Month==9 | Month==10 | Month==11 | Month==12),
    .default = "Unknown"
  )) %>%
  mutate(Academic_Year = paste(Year, Academic_Term, sep=" ")) %>%
  group_by(Database, Publisher, Platform, User_Activity, Academic_Term, Year)%>%
  summarize(Usage=sum(Usage))%>%
  ggplot(aes(Academic_Term, Usage, fill=factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(User_Activity ~.)
Graph6
















## End Graphing Section
