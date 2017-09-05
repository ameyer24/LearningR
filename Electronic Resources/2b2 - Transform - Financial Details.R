###############################################################################
# Cost Overview Functions _____________________________________________________
###############################################################################

# Summarize the change in cost for one database over time.
cost.database.change <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Database == DatabaseName) %>%
    filter(Fiscal_Year <= EndYear) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Price_Change = subfun.cost.diff(Cost, lag(Cost))) %>%
    mutate(Percent_Change = subfun.percent.cost.diff(Cost, lag(Cost))) %>%
    mutate(Cost = dollar(Cost)) %>%
    filter(Fiscal_Year >= StartYear) %>%
    subset(select = -c(2:5))
}

test = cost.database.change("New Testament Abstracts", 2011, 2018)


# Graphing database pricing.
cost.database.graph <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Database == DatabaseName) %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    ggplot(aes(x = Fiscal_Year, y = Cost)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    scale_y_continuous(labels=dollar) +
    xlab("Fiscal Year") +
    ylab("Cost of Subscription")
}
cost.database.graph ("Business Source Complete", 2013, 2018)
