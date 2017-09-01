###############################################################################
# Cost Overview Functions _____________________________________________________
###############################################################################


#############################################################################################################################################################################################################################################
# A sub-function to calculate the change in price.

# Handles NA values and no change better.
subfun.cost.diff <- function(Cost1,Cost2){
  cost.diff <- ifelse(Cost2 == 0 | is.na(Cost2),
                      0,
                      Cost1-Cost2)
  cost.diff <- dollar(cost.diff)
  return(cost.diff)
}

# A sub-function to calculate the percent change in cost.
# Handles NA values and no change better.
subfun.percent.cost.diff <- function(Cost1,Cost2){
  percent.cost.diff <- ifelse(Cost2 == 0 | is.na(Cost2),
                              0,
                              ((Cost1-Cost2)/Cost2))
  percent.cost.diff <- percent(percent.cost.diff)
  return(percent.cost.diff)
}
#############################################################################################################################################################################################################################################

# Summarize the change in cost for one database over time.
cost.4 <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year <= EndYear) %>%
    filter(Database == DatabaseName) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Price_Change = subfun.cost.diff(Cost, lag(Cost))) %>%
    mutate(Percent_Change = subfun.percent.cost.diff(Cost, lag(Cost))) %>%
    mutate(Cost = dollar(Cost)) %>%
    filter(Fiscal_Year > StartYear) %>%
    subset(select = -c(2:5)) %>%
    write_csv(paste(output.folder, "cost.4.csv",sep="/"))
}
test = cost.4("New Testament Abstracts", 2011, 2018)


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
test = cost.overview.5("New Testament Abstracts", 2012, 2018)

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