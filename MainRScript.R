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
## This creates a blank template to help with the import of pricing data.
DB_Pricing_Blank <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher,Platform, Year) %>%
  mutate(Price ="") %>%
  mutate(Notes = "") %>%
  spread(Year,Price) %>%
  write_csv(paste(export_folder, "DB_Pricing_Blank.csv",sep="/"))

## Imports the pricing information file.
Database_Pricing <- read_csv(paste(export_folder, "DB_Pricing.csv",sep="/"), col_names = TRUE)

## Some variables to describe the size and shape of the pricing data.
## four columns of descriptive information.
DB_Pricing_Description <- 4
## the rest are the years.
DB_Pricing_Years <- ncol(Database_Pricing)-DB_Pricing_Description


## Create "tidy" database pricing.
Tidy_Database_Pricing <- Database_Pricing %>%
  gather(Fiscal_Year, Cost, (DB_Pricing_Description +1):(ncol(Database_Pricing))) %>%
  mutate(Cost = as.numeric(Cost))

class(Tidy_Database_Pricing$Cost)

###############################
## Looking at pricing information.

## Create a simple table of pricing information.
Cost1 <- Tidy_Database_Pricing %>%
  group_by(Database) %>%
  spread(Fiscal_Year,Cost) %>%
  subset(select = -c(2,3))

## Sum cost by fiscal year
summary(Cost1$`2017`)

  
  
  
  
  
  
  
  
  
  







#############################
##Cost per use work
## Start with the fiscal year.
## Combine Summary 4_3 with Tidy_Database_Pricing
## Rename columns to match
CPU_Usage <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year))%>%
  group_by(Database, Publisher,Platform, User_Activity, Fiscal_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  rename(Description = User_Activity) %>%
  rename(Measure = Total_Usage)

CPU_Cost<- Database_Pricing %>%
  filter(Database =="Academic Search Complete") %>%
  gather(Fiscal_Year, Cost, Num_of_years:(Num_of_years + 3))%>%
  mutate(Cost = as.numeric(Cost)) %>%
  rename(Measure = Cost) %>%
  mutate(Description = "Total Cost") %>%
  select(-Notes)
  
CPU_Tidy <-  rbind.data.frame(CPU_Usage, CPU_Cost)

CPU_Tidy1 <- CPU_Tidy %>%
  filter(Database =="Academic Search Complete")

#############################
## GRAPHING USAGE

## Filter by database
Graph1 <- Tidy_DB1_data %>%
  filter(Database=="JSTOR") %>%
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
