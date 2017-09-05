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

###############################################################################
# Cost Overview Functions _____________________________________________________
###############################################################################

# Sums the cost of databases by fiscal year.
cost.1 <- function(StartYear, EndYear){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    group_by(Fiscal_Year) %>%
    summarize(Total_Cost= dollar(sum(Cost))) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.1.csv",sep="/"))
}
test = cost.1(2012,2018)

# Plot the total cost of databases by fiscal year.
cost.1.graph <- function(StartYear,EndYear){
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
cost.1.graph(2013,2018)

# Sums the cost of databases by fund and fiscal year.
cost.2 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.2.csv",sep="/"))
}
test = cost.2(2012,2018)

# Plot the cost of databases by fund and fiscal year.
cost.2.graph <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
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
cost.2.graph(2012,2018)

# Plot the cost of databases by fund and fiscal year.
# Funds act as different facets to the graph.
cost.2.graph.a <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= (sum(Cost))) %>%
    ggplot(aes(Total_Cost)) +
    geom_bar(aes(x=Fiscal_Year,
                 y=Total_Cost),
             stat="identity") +
    facet_grid(Fund ~ .) +
    ggtitle("Cost of Online Resources") +
    xlab("Fiscal Year") +
    ylab("Cost") +
    scale_y_continuous(labels = dollar)
}
cost.2.graph.a(2014,2018)

# Sums the cost of databases by fund and fiscal year.
cost.3 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    group_by(Fund, Fiscal_Year, Database) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.3.csv",sep="/"))
}
test = cost.3(2014,2018)

# Plot the cost of databases by fund and fiscal year.
# Funds as facets.
cost.3.graph <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    group_by(Fund, Fiscal_Year, Database) %>%
    summarize(Total_Cost = (sum(Cost))) %>%
    ggplot() +
    geom_point(aes(x=Fiscal_Year,y=Total_Cost)) +
    facet_grid(Fund ~ .) +
    ggtitle("Cost of Online Resources") +
    xlab("Fiscal Year") +
    ylab("Cost") +
    scale_y_continuous(labels = dollar)
}
cost.3.graph(2012,2018)

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
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    group_by(Database, Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.overview.7.csv",sep="/"))
}
test = cost.overview.7(2010,2018)

###############################################################################
# Cost Overview Functions _____________________________________________________
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

# test = cost.database.change("New Testament Abstracts", 2011, 2018)


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

