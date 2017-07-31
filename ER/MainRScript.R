# Overview
# This script was written to look at electronic resources usage.
# It looks at electronic resource usage (from Counter reports)
# It also uses pricing information entered by the user.

###############################################################################
# Installing and Loading Packages _____________________________________________
###############################################################################
install.packages("tidyverse")
install.packages("readxl")
install.packages("xlsx")
install.packages("zoo")
install.packages("mosaic")
install.packages("scales")
library(mosaic)
library(xlsx)
library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)
library(scales)

###############################################################################
# Reading the files and tidying the data ______________________________________
###############################################################################

# Setting up the folders
input <- "C:/DataScience/inputs"
output <- "C:/DataScience/outputs"
DB1folder <- "C:/DataScience/inputs/DB1Reports"
JR1folder <- "C:/DataScience/inputs/JR1Reports"

###############################################################################
# Import Database1 Usage Information___________________________________________
###############################################################################

# Defining functions to load the data from DB1 Reports.
load_CSV_DB1 <- function(path) { 
  csv_files <- dir(path, pattern = "*.(CSV|csv)", full.names = TRUE)
  tables <- lapply(csv_files, function(file){
    file %>%
      read_csv(skip=7, col_names = TRUE) %>%
      subset(select = -c(5)) %>%
      gather(Date, Usage, -c(1:4)) %>%
      mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
      mutate(Usage = as.numeric(Usage)) %>%
      plyr::rename(replace = c("User Activity" = "User_Activity"))
  })
  do.call(rbind, tables)
}

load_excel_DB1 <- function(path) { 
  excel_files <- dir(path, pattern = "*.xl*", full.names = TRUE)
  tables <- lapply(excel_files, function(file){
    file %>%
      read_excel(skip=7, col_names = TRUE) %>%
      subset(select = -c(5)) %>%
      gather(Date, Usage, -c(1:4)) %>%
      mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
      mutate(Usage = as.numeric(Usage)) %>%
      plyr::rename(replace = c("User Activity" = "User_Activity"))
  })
  do.call(rbind, tables)
}
# Creates dataframe with Database usage in a tidy format.
Tidy_DB1_data <-unique(rbind(load_CSV_DB1(DB1folder),load_excel_DB1(DB1folder)))

# See what information we have for what databases.
DB1.summary <- Tidy_DB1_data %>%
  group_by(Platform, Date) %>%
  summarise(Total_Usage = sum(Usage)) %>%
  spread(Date,Total_Usage)




###############################################################################
# Import Journal  Usage Information____________________________________________
###############################################################################
# Defining Functions to Load JR1 Data.
load_CSV_JR1 <- function(path) { 
  csv_files <- dir(path, pattern = "*.(CSV|csv)", full.names = TRUE)
  tables <- lapply(csv_files, function(file){
    file %>%
      read_csv(skip=7, col_names = TRUE) %>%
      slice(-1) %>%
      subset(select = -c(8,9,10)) %>%
      gather(Date, Usage, -c(1:7)) %>%
      mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
      mutate(Usage = as.numeric(Usage))
  })
  do.call(rbind, tables)
}

load_excel_JR1 <- function(path) { 
  excel_files <- dir(path, pattern = "*.xl*", full.names = TRUE)
  tables <- lapply(excel_files, function(file){
    file %>%
      read_excel(skip=7, col_names = TRUE) %>%
      slice(-1) %>%
      subset(select = -c(8,9,10)) %>%
      gather(Date, Usage, -c(1:7)) %>%
      mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
      mutate(Usage = as.numeric(Usage))
  })
  do.call(rbind, tables)
}

# Creates dataframe with journal usage in a tidy format.
Tidy_JR1_data <-unique(rbind(load_CSV_JR1(JR1folder),load_excel_JR1(JR1folder)))

JR1.summary <- Tidy_JR1_data %>%
  group_by(Platform, Date) %>%
  summarise(Total_Usage = sum(Usage)) %>%
  spread(Date,Total_Usage)

###############################################################################
# Import Pricing Information___________________________________________________
###############################################################################

# This creates a blank template to help with the import of pricing data.
DB_Pricing_Blank <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher,Platform, Year) %>%
  mutate(Price ="",
         Notes="",
         Fund="",
         Ordering_Site="",
         Order_Detail="Fiscal Year") %>%
  spread(Year,Price) %>%
  write_csv(paste(output, "DB_Pricing_Blank.csv",sep="/"))

# Imports the pricing information file.
Raw_DB_Pricing <- read_csv(paste(input, "DB_Pricing.csv",sep="/"),
                           col_names = TRUE)

# Creates a variable to describe the pricing data information. 
# This sets the number of descriptive columns at 7 (the rest are years)
DB_Pricing_Desc <- 7

# Creates a tidy dataframe of just database pricing.
# Keeps only the notes and fund information.
# Excludes databases without pricing.
Tidy_DB_Pricing <- Raw_DB_Pricing %>%
  gather(Fiscal_Year, Cost, (DB_Pricing_Desc +1):(ncol(Raw_DB_Pricing))) %>%
  filter(!is.na(Cost)) %>%
  subset(select = -c(6:7))



###############################################################################
# Summarize and Transform Usage Data __________________________________________
###############################################################################
# See what publishers we have in the dataset.
unique(c(Tidy_DB1_data$Publisher))
#See what platforms we have in the dataset.
unique(c(Tidy_DB1_data$Platform))
# See what databases we have in the dataset.
unique(c(Tidy_DB1_data$Database))

# Summarize database usage data by spreading into a "Counter" like report.
DBSummary1 <- Tidy_DB1_data %>%
  filter(Date >= 2010) %>% # Update this filter to customize date ranges.
  spread(Date, Usage, convert=TRUE) %>%
  arrange(Database,Platform) %>%
  write_csv(paste(output, "DBSummary1.csv",sep="/"))

# Summarize database usage data by  the Calendar Year
DBSummary2 <- Tidy_DB1_data %>%
  mutate(Year = year(Date)) %>%
  group_by(Database, Publisher, User_Activity, Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Year, Total_Usage) %>%
  write_csv(paste(output, "DBSummary2.csv",sep="/"))

# Summarize database usage data by the Academic Year
DBSummary3 <- Tidy_DB1_data %>%
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
  write_csv(paste(output, "DBSummary3.csv",sep="/"))

# Summarize database usage data by the Fiscal Year
DBSummary4 <- Tidy_DB1_data %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year)) %>%
  mutate(Fiscal_Year = paste("FY", Fiscal_Year, sep=" ")) %>%
  group_by(Database, Publisher, User_Activity, Fiscal_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Fiscal_Year, Total_Usage) %>%
  write_csv(paste(output, "DBSummary4.csv",sep="/"))

# Summarize database usage data by the Fiscal Year
# Does not spread the data by year.
DBSummary4_1 <- Tidy_DB1_data %>%
  filter(Database =="Business Source Complete") %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year)) %>%
  group_by(User_Activity, Fiscal_Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(User_Activity, Total_Usage) %>%
  write_csv(paste(output, "DBSummary4_1.csv",sep="/"))


# Summarize journal usage data by spreading into a "Counter" like report.
JRSummary1 <- Tidy_JR1_data %>%
  filter(Date >= 2012) %>% # Update this filter to create customized date ranges.
  spread(Date, Usage, convert=TRUE) %>%
  arrange(Journal,Platform) %>%
  write_csv(paste(output, "JRSummary1.csv",sep="/"))

# Summarize journal usage data by grouping by platform and year.
JRSummary2 <- Tidy_JR1_data %>%
  filter(Date >= 2014) %>%
  mutate(Year = year(Date)) %>%
  group_by(Platform, Year) %>%
  summarize(Total_Usage= sum(Usage)) %>%
  spread(Year, Total_Usage) %>%
  write_csv(paste(output, "JRSummary2.csv",sep="/"))





###############################################################################
# Summarize and Transform  Pricing Information_________________________________
###############################################################################

# Create a simple table of pricing information.
Cost1 <- Tidy_DB_Pricing %>%
  filter(Fiscal_Year > 2013, Fiscal_Year < 2018) %>%
  mutate(Cost = dollar_format()(Cost)) %>%
  spread(Fiscal_Year, Cost) %>%
  write_csv(paste(output, "cost1.csv",sep="/"))

# Calculates Change in price (raw and percent) over time.
Cost2 <- Tidy_DB_Pricing %>%
  filter(Fiscal_Year < 2018) %>%
  arrange(Database, Fiscal_Year) %>%
  group_by(Database) %>%
  mutate(Cost_Last_FY=lag(Cost)) %>%
  mutate(Change_In_Price = Cost-Cost_Last_FY) %>%
  mutate(Change_In_Price_Percent = (Cost-Cost_Last_FY)/Cost_Last_FY) %>%
  mutate(Change_In_Price_Percent = paste(round((Change_In_Price_Percent * 100), digits=2),"%",sep=""))

# Creates a table that just shows price changes in percents.
Cost3 <- Tidy_DB_Pricing %>%
  filter(Fiscal_Year < 2018) %>%
  arrange(Database, Fiscal_Year) %>%
  group_by(Database) %>%
  mutate(Change_In_Price = Cost-lag(Cost), Change_In_Price_Percent = (Cost-lag(Cost))/lag(Cost)) %>%
  mutate(Change_In_Price_Percent = paste(round((Change_In_Price_Percent * 100), digits=2),"%",sep="")) %>%
  filter(Fiscal_Year > 2013) %>%
  subset(select = -c(2:4)) %>%
  subset(select= -c(4:5)) %>%
  spread(Fiscal_Year,Change_In_Price_Percent)
  
# Create a report for one database that shows cost and changes.
Cost4 <- Tidy_DB_Pricing %>%
  filter(Fiscal_Year < 2018) %>%
  arrange(Database, Fiscal_Year) %>%
  group_by(Database) %>%
  mutate(Change_In_Price = Cost-lag(Cost)) %>%
  mutate(Change_In_Price_Percent = (Cost-lag(Cost))/lag(Cost)) %>%
  filter(Fiscal_Year > 2013) %>%
  subset(select = -c(2:5)) 

# Creates a simple bar chart for individual database costs over time.
CostGraph1 <- Tidy_DB_Pricing %>%
  filter(Database=="Business Source Complete") %>%
  filter(Fiscal_Year > 2013, Fiscal_Year < 2018) %>%
  mutate(Cost = dollar_format()(Cost)) %>%
  ggplot(aes(x=Fiscal_Year,y=Cost, label=Cost)) +
  geom_bar(stat="identity", fill="darkgreen")

CostGraph1 +
  ggtitle("Cost of Subscription over Time") +
  geom_label(aes(label=Cost), vjust=3)

## Breakdown by Fund and Year
## work in progress
CostGraph2 <- Tidy_DB_Pricing %>%
  filter(Fiscal_Year > 2014, Fiscal_Year < 2018) %>%
  filter(!is.na(Cost))%>%
  group_by(Fund, Fiscal_Year, Database) %>%
  summarize(Total_Cost=sum(Cost))%>%
  ggplot(aes(x=Fiscal_Year,y=Total_Cost, fill=Fund)) + 
  geom_bar(stat="identity",width = 1) +
  facet_grid(Fund ~ .)
CostGraph2

###############################################################################
# Combine Usage and Cost! _____________________________________________________
# Calculating Cost Per Use ____________________________________________________
###############################################################################

# Starting points: the two "tidy" dataframes (Use and Pricing)
# Summarizes use on the fiscal year.
CPU1_Use <- Tidy_DB1_data %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month >6, Year + 1,Year)) %>%
  group_by(Database, Publisher,Platform, User_Activity, Fiscal_Year) %>%
  summarize(Total_Usage= sum(Usage))

CPU1_Cost <- Tidy_DB_Pricing %>%
  subset(select = -c(4:5))

# Merge Use and Cost dataframes by Database, Publisher, Platform, and Fiscal Year.
CPU_Combined <- merge(CPU1_Use, CPU1_Cost, by=c("Database","Publisher","Platform","Fiscal_Year"))

# Adds new column that calculates cost per user action.
CPU_Combined$Cost_Per_User_Action <- CPU_Combined$Cost/CPU_Combined$Total_Usage
# Writes to CSV file.
write_csv(CPU_Combined, paste(output, "Cost_Per_Use.csv",sep="/"))


Cost_Per_Use1 <- CPU_Combined %>%
  subset(select=-c(6:7)) %>%
  filter(Fiscal_Year > 2015) %>%
  spread(User_Activity,Cost_Per_User_Action) %>%
  write_csv(paste(output, "Cost_Per_Use1.csv",sep="/"))

###############################################################################
# Graphing Usage_______________________________________________________________
###############################################################################

# Graphs usage by individual database
Graph1 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  ggplot() +
  geom_line(mapping = aes(x=Date, y=Usage, color=User_Activity)) +
  scale_x_yearmon()
Graph1

# Graphs usage by individual database and user activity
Graph2 <- Tidy_DB1_data %>%
  filter(User_Activity=="Record Views") %>%
  filter(Database=="Nursing Reference Center") %>%
  ggplot(aes(Date, Usage)) +
  geom_line(mapping = aes(color=Database)) +
  geom_smooth(span=0.7) +
  scale_x_yearmon()
Graph2

# Graphs usage by individual database; facet by user activity
Graph2_1 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  ggplot(aes(Date, Usage)) +
  geom_line() +
  geom_smooth(span=0.7) +
  scale_x_yearmon() +
  facet_grid(User_Activity ~ .)
Graph2_1

# Graphs usage by individual database; facet by user activity, scales free.
Graph2_2 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  ggplot(aes(Date, Usage)) +
  geom_line() +
  geom_smooth(span=0.7) +
  scale_x_yearmon() +
  facet_grid(User_Activity ~ ., scales = "free")
Graph2_2

# Graphs usage by individual database, summarized by calendar year.
Graph3 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  mutate(Year=year(Date)) %>%
  group_by(Database, Publisher, Platform, User_Activity, Year)%>%
  summarize(Usage=sum(Usage)) %>%
  ggplot() +
  geom_line(mapping = aes(x=Year, y=Usage, color=User_Activity))
Graph3

# Graphs usage by individual database, summarized by academic term.
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

# Graphs usage by individual database, summarized by academic term.
# User_Activity is faceted.
Graph5 <- Tidy_DB1_data %>%
  filter(Database=="Nursing Reference Center") %>%
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

## Bar Graph grouped by academic term and then year.
Graph6 <- Tidy_DB1_data %>%
  filter(Database=="Business Source Complete") %>%
  filter(User_Activity=="Record Views") %>%
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

## Bar Graph grouped by year and then academic term.
Graph7 <- Tidy_DB1_data %>%
  filter(Database=="Nursing Reference Center") %>%
  filter(User_Activity=="Record Views") %>%
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
  ggplot(aes(Year, Usage, fill=factor(Academic_Term))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(User_Activity ~.)
Graph7

###############################################################################
# A Functional Approach _______________________________________________________
###############################################################################
# I'm going to explore writing functions to do some reporting.
# I'm repeating code I used above. Want to do this independently as possible.
# This seems like a better approach. Going to continue doing this.

###############################################################################
# Cost Overview Functions _____________________________________________________
###############################################################################
# Sums the  cost of databases by fiscal year.
cost.overview.1 <- function(StartYear,EndYear){
  Tidy_DB_Pricing %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output, "cost.overview.1.csv",sep="/"))
}
test = cost.overview.1(2012,2018)

# Sums the  cost of databases by fund and fiscal year.
cost.overview.2 <- function(StartYear,EndYear){
  Tidy_DB_Pricing %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output, "cost.overview.2.csv",sep="/"))
}
test = cost.overview.2(2012,2018)

# Sums the cost of databases by fund and fiscal year.
cost.overview.3 <- function(StartYear,EndYear){
  Tidy_DB_Pricing %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fund, Fiscal_Year, Database) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output, "cost.overview.3.csv",sep="/"))
}
test = cost.overview.3(2012,2018)

# A sub-function to calculate the change in price.
# Handles NA values and no change better.
subfun.cost.diff <- function(Cost1,Cost2){
  cost.diff <- ifelse(Cost2 == 0 | is.na(Cost2),
                      Cost1,
                      Cost1-Cost2)
  return(cost.diff)
}


# Summarize the change in cost for one database over time.
cost.overview.4 <- function(DatabaseName,StartYear,EndYear){
  Tidy_DB_Pricing %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Database == DatabaseName) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Change_In_Price = subfun.cost.diff(Cost, lag(Cost))) %>%
    filter(Fiscal_Year > StartYear) %>%
    subset(select = -c(2:5)) %>%
    write_csv(paste(output, "cost.overview.4.csv",sep="/"))
}
test = cost.overview.4("Morningstar Investment Research Center", 2011, 2018)

# A sub-function to calculate the percent change in cost.
# Handles NA values and no change better.
subfun.percent.cost.diff <- function(Cost1,Cost2){
  percent.cost.diff <- ifelse(Cost2 == 0 | is.na(Cost2),
                      0,
                      (Cost1-Cost2)/Cost2)
  return(percent.cost.diff)
}

# Summarize the change in cost for one database over time.
cost.overview.5 <- function(DatabaseName,StartYear,EndYear){
  Tidy_DB_Pricing %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Database == DatabaseName) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Percent_Change = subfun.percent.cost.diff(Cost, lag(Cost))) %>%
    filter(Fiscal_Year > StartYear) %>%
    subset(select = -c(2:5)) %>%
    write_csv(paste(output, "cost.overview.5.csv",sep="/"))
}
test = cost.overview.5("Morningstar Investment Research Center", 2012, 2018)

# Summarize the change in cost for each database over time.
cost.overview.6 <- function(StartYear,EndYear){
  Tidy_DB_Pricing %>%
    filter(Fiscal_Year < EndYear) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Percent_Change = subfun.percent.cost.diff(Cost, lag(Cost))) %>%
    filter(Fiscal_Year > StartYear) %>%
    spread(Fiscal_Year, Percent_Change) %>%
    subset(select = -c(2:6)) %>%
    write_csv(paste(output, "cost.overview.6.csv",sep="/"))
}
test = cost.overview.6(2012, 2018)

###############################################################################
# Cost Functions ______________________________________________________________
###############################################################################


# Graphing database pricing.
cost.graph.1 <- function(DatabaseName){
  Tidy_DB_Pricing %>%
    filter(Database==DatabaseName) %>%
    filter(Fiscal_Year > 2013, Fiscal_Year < 2018) %>%
    ggplot(aes(x=Fiscal_Year, y=Cost, label=Cost)) +
    geom_bar(stat="identity", fill="darkgreen") +
    ggtitle(paste(DatabaseName)) +
    xlab("Fiscal Year") +
    ylab("Cost of Subscription")
}
cost.graph.1("Business Source Complete")

# Graphing database usage.
usage.graph.1 <- function(DatabaseName) {
  Tidy_DB1_data %>%
    filter(Database==DatabaseName) %>%
    ggplot(aes(Date, Usage)) +
    geom_line() +
    geom_smooth(span=0.7) +
    scale_x_yearmon() +
    facet_grid(User_Activity ~ ., scales = "free")
}
usage.graph.1("Business Source Complete")

# Graphing database usage.
usage.graph.2 <- function(DatabaseName) {
  Tidy_DB1_data %>%
    filter(Database == DatabaseName) %>%
    filter(User_Activity != "Searches-federated and automated") %>%
    ggplot(aes(Date, Usage)) +
    geom_line() +
    geom_smooth(span=0.7) +
    scale_x_yearmon() +
    facet_grid(User_Activity ~ .)
}
usage.graph.2("Business Source Complete")

# Graphing database usage.
usage.graph.3 <- function(DatabaseName) {
  Tidy_DB1_data %>%
    filter(Database == DatabaseName) %>%
    filter(User_Activity != "Searches-federated and automated") %>%
    mutate(Year = year(Date), Month=month(Date)) %>%
    filter(Year>2014) %>%
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
    facet_grid(User_Activity ~ .) + 
    geom_line(aes(group=User_Activity))
}
usage.graph.3("Business Source Complete")

# Reporting Cost per Use
cost.per.use.report.1 <- function(DatabaseName){
  CPU_Combined %>%
    filter(Database==DatabaseName) %>%
    subset(select=-c(6:7)) %>%
    spread(User_Activity, Cost_Per_User_Action) %>%
    subset(select=-c(1:3)) %>%
    write_csv(paste(output, "cost.per.use.report.1.csv",sep="/"))
}
cost.per.use.report.1("Business Source Complete")
