###############################################################################
# Cost Overview Functions _____________________________________________________
###############################################################################

# Sums the cost of databases by fiscal year.
cost.1 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fiscal_Year) %>%
    summarize(Total_Cost= dollar(sum(Cost))) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.1.csv",sep="/"))
}
test = cost.1(2012,2018)

# Plot the total cost of databases by fiscal year.
cost.1.graph <- function(StartYear,EndYear){
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
cost.1.graph(2013,2018)

# Sums the cost of databases by fund and fiscal year.
cost.2 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
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
cost.2.graph(2012,2018)

# Plot the cost of databases by fund and fiscal year.
# Funds act as different facets to the graph.
cost.2.graph.a <- function(StartYear,EndYear){
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
cost.2.graph.a(2012,2018)

# Sums the cost of databases by fund and fiscal year.
cost.3 <- function(StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Fund, Fiscal_Year, Database) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.3.csv",sep="/"))
}
test = cost.3(2011,2018)

# Plot the cost of databases by fund and fiscal year.
# Funds as facets.
cost.3.graph <- function(StartYear,EndYear){
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
    filter(Fiscal_Year < EndYear) %>%
    filter(Fiscal_Year > StartYear) %>%
    group_by(Database, Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output.folder, "cost.overview.7.csv",sep="/"))
}
test = cost.overview.7(2010,2018)
