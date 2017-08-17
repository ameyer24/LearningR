# This script looks at electronic resources usage and pricing information.
# It ingests DB1 and JR1 Counter Reports (Version 4)
# It allows libraries to import pricing information.
# It then calculates cost per use and other summaries.

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
input.folder <- "C:/DataScience/inputs"
output.folder <- "C:/DataScience/outputs"
DB1folder <- "C:/DataScience/inputs/DB1Reports"
JR1folder <- "C:/DataScience/inputs/JR1Reports"

###############################################################################
# Import Database Usage Information____________________________________________
###############################################################################

# Defining functions to load the data from DB1 Reports.
load.DB1.csv <- function(path) { 
  csv.files <- dir(path, pattern = "*.(CSV|csv)", full.names = TRUE)
  tables <- lapply(csv.files, function(file){
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

load.DB1.excel <- function(path) { 
  excel.files <- dir(path, pattern = "*.xl*", full.names = TRUE)
  tables <- lapply(excel.files, function(file){
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
DB1 <-unique(rbind(load.DB1.csv(DB1folder),load.DB1.excel(DB1folder)))

# Get an overview of our data to make sure things imported relatively well.
DB1.summary <- DB1 %>%
  group_by(Publisher, Date) %>%
  summarise(Total_Usage = sum(Usage)) %>%
  spread(Date,Total_Usage) %>%
  write_csv(paste(output.folder, "DB1.usage.summary.csv",sep="/"))

###############################################################################
# Import Journal Usage Information____________________________________________
###############################################################################
# Defining Functions to Load JR1 Data.
load.JR1.csv <- function(path) { 
  csv.files <- dir(path, pattern = "*.(CSV|csv)", full.names = TRUE)
  tables <- lapply(csv.files, function(file){
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

load.JR1.excel <- function(path) { 
  excel.files <- dir(path, pattern = "*.xl*", full.names = TRUE)
  tables <- lapply(excel.files, function(file){
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
JR1 <-unique(rbind(load.JR1.csv(JR1folder),load.JR1.excel(JR1folder)))

JR1.summary <- JR1 %>%
  group_by(Platform, Date) %>%
  summarise(Total_Usage = sum(Usage)) %>%
  spread(Date,Total_Usage)

###############################################################################
# Import Pricing Information___________________________________________________
###############################################################################

# This creates a blank template to help with the import of pricing data.
db.price.template <- DB1 %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher,Platform, Year) %>%
  mutate(Price ="",
         Notes="",
         Fund="",
         Ordering_Site="",
         Order_Detail="Fiscal Year") %>%
  spread(Year,Price) %>%
  write_csv(paste(output.folder, "database.price.template.csv",sep="/"))

# Imports the pricing information file.
db.prices.raw <- read_csv(paste(input.folder, "database.price.csv",sep="/"),col_names = TRUE)

# Creates a variable to describe the pricing data information. 
# This sets the number of descriptive columns at 7 (the rest are years)
db.prices.desc <- 7

# Creates a tidy dataframe of just database pricing.
# Keeps only the notes and fund information.
# Excludes databases without pricing.
DB1.fin <- db.prices.raw %>%
  gather(Fiscal_Year, Cost, (db.prices.desc +1):(ncol(db.prices.raw))) %>%
  filter(!is.na(Cost)) %>%
  mutate(Cost = as.numeric(Cost)) %>%
  subset(select = -c(6:7))


###############################################################################
# Cost Overview Functions _____________________________________________________
###############################################################################

# Sums the  cost of databases by fiscal year.
# Very upper level statistical view.
cost.overview.1 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fiscal_Year) %>%
    summarize(Total_Cost= dollar(sum(Cost))) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.overview.1.csv",sep="/"))
}
test = cost.overview.1(2012,2018)

# Plot the total cost of online resources over time.
cost.overview.1.graph <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fiscal_Year) %>%
    summarize(Total_Cost= (sum(Cost))) %>%
    ## I have the right data.
    ## Now graph the stuff and make it look nice.
    ggplot(aes(Total_Cost)) +
    geom_bar(aes(Fiscal_Year, Total_Cost),stat="identity", fill="darkgreen") +
    ggtitle("Cost of Online Resources") +
    xlab("Fiscal Year") +
    ylab("Cost") +
    scale_y_continuous(labels = scales::dollar)
}
cost.overview.1.graph(2013,2018)


# Sums the  cost of databases by fund and fiscal year.
cost.overview.2 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.overview.2.csv",sep="/"))
}
test = cost.overview.2(2012,2018)

# Plot this data.
cost.overview.2.graph <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= (sum(Cost))) %>%
    ## I have the right data.
    ## Now graph the stuff and make it look nice.
    ggplot(aes(Total_Cost)) +
    geom_bar(aes(x=Fiscal_Year,
                 y=Total_Cost,
                 fill=Fund)
             ,stat="identity") +
    ggtitle("Cost of Online Resources") +
    xlab("Fiscal Year") +
    ylab("Cost") +
    scale_y_continuous(labels = scales::dollar)
}
cost.overview.2.graph(2012,2018)

# Plot this data.
# Funds as facets.
cost.overview.2f.graph <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= (sum(Cost))) %>%
    ## I have the right data.
    ## Now graph the stuff and make it look nice.
    ggplot(aes(Total_Cost)) +
    geom_bar(aes(x=Fiscal_Year,
                 y=Total_Cost),
             stat="identity") +
    facet_grid(Fund ~ .) +
    ggtitle("Cost of Online Resources") +
    xlab("Fiscal Year") +
    ylab("Cost") +
    scale_y_continuous(labels = scales::dollar)
}
cost.overview.2f.graph(2012,2018)

# Sums the cost of databases by fund and fiscal year.
cost.overview.3 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fund, Fiscal_Year, Database) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.overview.3.csv",sep="/"))
}
test = cost.overview.3(2012,2018)

# Plot this data.
# Funds as facets.
cost.overview.3.graph <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fund, Fiscal_Year, Database) %>%
    summarize(Total_Cost = (sum(Cost))) %>%
    ## I have the right data.
    ## Now graph the stuff and make it look nice.
    ggplot() +
    geom_point(aes(x=Fiscal_Year,y=Total_Cost)) +
    facet_grid(Fund ~ .) +
    ggtitle("Cost of Online Resources") +
    xlab("Fiscal Year") +
    ylab("Cost") +
    scale_y_continuous(labels = scales::dollar)
}
cost.overview.3.graph(2012,2018)

# A sub-function to calculate the change in price.
# Handles NA values and no change better.
subfun.cost.diff <- function(Cost1,Cost2){
  cost.diff <- ifelse(Cost2 == 0 | is.na(Cost2),
                      dollar(Cost1),
                      dollar(Cost1-Cost2))
  return(cost.diff)
}


# Summarize the change in cost for one database over time.
cost.overview.4 <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Database == DatabaseName) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Change_In_Price = subfun.cost.diff(Cost, lag(Cost))) %>%
    mutate(Cost = dollar(Cost)) %>%
    filter(Fiscal_Year > StartYear) %>%
    subset(select = -c(2:5)) %>%
    write_csv(paste(output.folder, "cost.overview.4.csv",sep="/"))
}
test = cost.overview.4("Morningstar Investment Research Center", 2011, 2018)

# A sub-function to calculate the percent change in cost.
# Handles NA values and no change better.
subfun.percent.cost.diff <- function(Cost1,Cost2){
  percent.cost.diff <- ifelse(Cost2 == 0 | is.na(Cost2),
                              percent(0),
                              percent((Cost1-Cost2)/Cost2))
  return(percent.cost.diff)
}

# Summarize the change in cost for one database over time.
cost.overview.5 <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Database == DatabaseName) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Percent_Change = subfun.percent.cost.diff(Cost, lag(Cost))) %>%
    filter(Fiscal_Year > StartYear) %>%
    subset(select = -c(2:5)) %>%
    write_csv(paste(output.folder, "cost.overview.5.csv",sep="/"))
}
test = cost.overview.5("Morningstar Investment Research Center", 2012, 2018)

cost.overview.6 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Percent_Change = subfun.percent.cost.diff(Cost, lag(Cost))) %>%
    filter(Fiscal_Year > StartYear) %>%
    subset(select = -c(2:5,7)) %>%
    spread(Fiscal_Year, Percent_Change) %>%
    write_csv(paste(output.folder, "cost.overview.6.csv",sep="/"))
}
test = cost.overview.6(2012, 2018)

# Sums the  cost of databases by fund and fiscal year.
cost.overview.7 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Database, Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.overview.7.csv",sep="/"))
}
test = cost.overview.7(2010,2018)


# Graphing database pricing.
cost.overview.8 <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Database==DatabaseName) %>%
    filter(Fiscal_Year > StartYear, Fiscal_Year < EndYear) %>%
    ggplot(aes(x=Fiscal_Year, y=Cost, label=Cost)) +
    geom_bar(stat="identity", fill="darkgreen") +
    ggtitle(paste(DatabaseName)) +
    xlab("Fiscal Year") +
    ylab("Cost of Subscription")
}
cost.overview.8("Business Source Complete", 2013, 2018)

###############################################################################
# Usage Overivew Functions ____________________________________________________
###############################################################################

# Graphing database usage.
usage.graph.1 <- function(DatabaseName,StartYear,EndYear){
  DB1 %>%
    filter(Database==DatabaseName) %>%
    filter(Date > StartYear, Date < EndYear) %>%
    ggplot(aes(Date, Usage)) +
    geom_line() +
    geom_smooth(span=0.7) +
    scale_x_yearmon() +
    facet_grid(User_Activity ~ ., scales = "free")
}
usage.graph.1("Academic Search Complete", 2015, 2018)

# Graphing database usage.
usage.graph.2 <- function(DatabaseName,StartYear,EndYear){
  DB1 %>%
    filter(Database == DatabaseName) %>%
    filter(Date > StartYear, Date < EndYear) %>%
    filter(User_Activity != "Searches-federated and automated") %>%
    ggplot(aes(Date, Usage)) +
    geom_line() +
    geom_smooth(span=0.7) +
    scale_x_yearmon() +
    facet_grid(User_Activity ~ .)
}
usage.graph.2("Business Source Complete", 2015, 2018)

# Graphing database usage.
usage.graph.3 <- function(DatabaseName,StartYear,EndYear){
  DB1 %>%
    filter(Database == DatabaseName) %>%
    filter(Date > StartYear, Date < EndYear) %>%
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
    # Graphs the data on a line graph.
    ggplot(aes(Academic_Year,Usage)) +
    facet_grid(User_Activity ~ .) + 
    geom_line(aes(group=User_Activity))
}
usage.graph.3("Business Source Complete", 2015, 2018)


###############################################################################
# Cost Per Use - Set Up________________________________________________________
###############################################################################

# Need to combine usage data and cost data.
# summarized usage on the fiscal year.
cost.per.use.usage <- DB1 %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month > 6, Year + 1,Year)) %>%
  group_by(Database, Publisher,Platform, User_Activity, Fiscal_Year) %>%
  summarize(Usage = sum(Usage)) %>%
  na.omit()

# Transform the database pricing dataframe
cost.per.use.cost <- select(DB1.fin, -c(4:5))

# Merging everything together.
# Including everything from the usage dataframe.
cost.per.use <- merge(cost.per.use.usage, cost.per.use.cost, all.cost.per.use.usage = TRUE)

# Adds new column that calculates cost per user action.
cost.per.use$Cost_Per_Action <- cost.per.use$Cost/cost.per.use$Usage

# Replace all non-finite values with 0
# This solves the NaN and Inf problems introduced in the last step.
cost.per.use$Cost_Per_Action[!is.finite(cost.per.use$Cost_Per_Action)] <- 0

# Makes the currency things looks nicer.
cost.per.use$Cost <- dollar(cost.per.use$Cost)
cost.per.use$Cost_Per_Action <- dollar(cost.per.use$Cost_Per_Action)

###############################################################################
# Cost Per Use Functions ____________________________________________________
###############################################################################

cpu.overview.11 <- function(df){
  df %>%
  select(-c(2:3,6:7)) %>%
  spread(Fiscal_Year, Cost_Per_Action)
}

test <- cpu.overview.1(cost.per.use)

cpu.overview.2 <- function(df){
  df %>%
    filter(Fiscal_Year > 2013) %>%
    filter(User_Activity == "Result Clicks") %>%
    select(-c(2:3,6:7)) %>%
    spread(Fiscal_Year, Cost_Per_Action)
}

test <- cpu.overview.2(cost.per.use)
