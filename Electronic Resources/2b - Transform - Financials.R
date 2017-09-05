###############################################################################
# Financial Subfunctions ______________________________________________________
###############################################################################
###############################################################################

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

###############################################################################
# Financial Overview Functions ________________________________________________
###############################################################################

# Sums the cost of databases by fiscal year.
cost.overview.table <- function(StartYear, EndYear){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    group_by(Fiscal_Year) %>%
    summarize(Total_Cost= dollar(sum(Cost))) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.1.csv",sep="/"))
}
test = cost.overview.table(2012,2018)

# Plot the total cost of databases by fiscal year.
cost.overview.graph <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    group_by(Fiscal_Year) %>%
    summarize(Total_Cost= (sum(Cost))) %>%
    ggplot(aes(Total_Cost)) +
    geom_bar(aes(Fiscal_Year, Total_Cost),stat="identity", fill="darkgreen") +
    ggtitle("Cost of Online Resources") +
    xlab("Fiscal Year") +
    ylab("Cost") +
    scale_y_continuous(labels = dollar)
}
cost.overview.graph(2013,2018)

# Sums the cost of databases by fund and fiscal year.
cost.overview.table.fund <- function(StartYear,EndYear,SelectFund = all.funds){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(Fund %in% SelectFund) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.2.csv",sep="/"))
}
test = cost.overview.table.fund(2014,2018)

# Plot the cost of databases by fund and fiscal year.
# Accepts an option parameter to specify fund; defaults to all funds.
cost.overview.table.fund <- function(StartYear,EndYear,SelectFund = all.funds){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(Fund %in% SelectFund) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= (sum(Cost))) %>%
    ggplot(aes(Total_Cost)) +
    geom_bar(aes(x=Fiscal_Year,
                 y=Total_Cost,
                 fill=Fund)
             ,stat="identity") +
    ggtitle("Cost of Online Resources") +
    xlab("Fiscal Year") +
    ylab("Cost") +
    scale_y_continuous(labels = dollar)
}
cost.overview.table.fund(2014,2018)


# Cost of databases grouped by fund and fiscal year.
# Accepts an option parameter to specify fund; defaults to all funds.
cost.overview.table.db <- function(StartYear, EndYear, SelectFund = all.funds){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(Fund %in% SelectFund) %>%
    group_by(Fund, Fiscal_Year, Database) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.3.csv",sep="/"))
}
test = cost.overview.table.db(2014, 2018, "Seminary")


###############################################################################
# Financial Details per Database_______________________________________________
###############################################################################

# Summarize the change in cost for one database over time.
cost.database.change <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Database %in% DatabaseName) %>%
    filter(Fiscal_Year <= EndYear) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Price_Change = subfun.cost.diff(Cost, lag(Cost))) %>%
    mutate(Percent_Change = subfun.percent.cost.diff(Cost, lag(Cost))) %>%
    mutate(Cost = dollar(Cost)) %>%
    filter(Fiscal_Year >= StartYear) %>%
    subset(select = -c(2:5))
}

# Graphing database pricing.
cost.database.graph <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Database %in% DatabaseName) %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    ggplot(aes(x = Fiscal_Year, y = Cost)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    scale_y_continuous(labels=dollar) +
    xlab("Fiscal Year") +
    ylab("Cost of Subscription")
}
cost.database.graph ("Business Source Complete", 2013, 2018)

