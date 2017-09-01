###############################################################################
# Cost Overview Functions _____________________________________________________
###############################################################################


################################################################################
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
################################################################################

# Summarize the change in cost for one database over time.
cost.database.change <- function(DatabaseName,StartYear,EndYear){
  DB1.fin %>%
    filter(Fiscal_Year <= EndYear) %>%
    filter(Database == DatabaseName) %>%
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
    ggtitle(paste(DatabaseName)) +
    xlab("Fiscal Year") +
    ylab("Cost of Subscription")
}
cost.database.graph ("Business Source Complete", 2013, 2018)
