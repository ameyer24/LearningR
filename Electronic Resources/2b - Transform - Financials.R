###############################################################################
# Financial Subfunctions ______________________________________________________
###############################################################################
# A sub-function to calculate the change in price.
# Handles NA values and no change better.
cost_difference <- function(Cost1, Cost2){
  cost.diff <- ifelse(Cost2 == 0 | is.na(Cost2),
                      0,
                      Cost1-Cost2)
  cost.diff <- dollar(cost.diff)
  return(cost.diff)
}

# A sub-function to calculate the percent change in cost.
# Handles NA values and no change better.
percent_difference <- function(Cost1, Cost2){
  percent.cost.diff <- ifelse(Cost2 == 0 | is.na(Cost2),
                              0,
                              ((Cost1-Cost2)/Cost2))
  percent.cost.diff <- percent(percent.cost.diff)
  return(percent.cost.diff)
}

###############################################################################
# Financial Overview Functions ________________________________________________
###############################################################################

# Summarize the cost of databases by fund and fiscal year.
financial_summary_fund_FY <- function(StartYear,
                                     EndYear,
                                     SelectFund = all_funds){
  DB1_financial %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(Fund %in% SelectFund) %>%
    group_by(Fund, Fiscal_Year) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output_folder, "cost.2.csv",sep="/"))
}
test = financial_summary_fund_FY(2014,2018)


financial_summary_fund_FY_graph <- function(StartYear,
                                     EndYear,
                                     SelectFund = all_funds){
  DB1_financial %>%
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
financial_summary_fund_FY_graph(2014, 2018)

# Cost of databases grouped by fund and fiscal year.
# Accepts an option parameter to specify fund; defaults to all funds.
financial_details_fund_FY <- function(StartYear,
                                   EndYear,
                                   SelectFund = all_funds){
  DB1_financial %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(Fund %in% SelectFund) %>%
    group_by(Fund, Fiscal_Year, Database) %>%
    summarize(Total_Cost= sum(Cost)) %>%
    mutate(Total_Cost = dollar(Total_Cost)) %>%
    spread(Fiscal_Year, Total_Cost) %>%
    write_csv(paste(output_folder, "cost.3.csv",sep="/"))
}
test = financial_details_fund_FY(2014, 2018)


###############################################################################
# Financial Details per Database_______________________________________________
###############################################################################

# Summarize the change in cost for one database over time.
financial_change_database_FY <- function(DatabaseName,
                                 StartYear,
                                 EndYear){
  DB1_financial %>%
    filter(Database %in% DatabaseName) %>%
    filter(Fiscal_Year <= EndYear) %>%
    arrange(Fiscal_Year) %>%
    group_by(Database) %>%
    mutate(Price_Change = cost_difference(Cost, lag(Cost))) %>%
    mutate(Percent_Change = percent_difference(Cost, lag(Cost))) %>%
    mutate(Cost = dollar(Cost)) %>%
    filter(Fiscal_Year >= StartYear) %>%
    select(Database,Fiscal_Year,Cost,Price_Change,Percent_Change) %>%
    rename("Fiscal Year" = "Fiscal_Year",
           "Price Change" = "Price_Change",
           "Percent Change" = "Percent_Change")
}

test <- financial_change_database_FY("Business Source Complete", 2013, 2018)
test <- cost.database.change(c("Business Source Complete","New Testament Abstracts"), 2013, 2018)

# Graphing database pricing.
financial_change_database_FY_graph <- function(DatabaseName,
                                               StartYear,
                                               EndYear){
  DB1_financial %>%
    filter(Database %in% DatabaseName) %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    ggplot(aes(x = Fiscal_Year, y = Cost)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    scale_y_continuous(labels=dollar) +
    xlab("Fiscal Year") +
    ylab("Cost of Subscription")
}
financial_change_database_FY_graph("Business Source Complete", 2013, 2018)

###############################################################################
# Other Financial Functions ___________________________________________________
###############################################################################

# Function to return Fund from DB1.fin given database name.
get_fund <-function(DatabaseName) {
  DB1_financial %>%
    filter(Database %in% DatabaseName) %>%
    select(Fund) %>%
    unique() %>%
    return()
}

# Function to return all the resources charged to a given fund
get_databases <- function(FundName){
  DB1_financial %>%
    filter(Fund %in% FundName) %>%
    select(Database) %>%
    unique() %>%
    return()
}
get_fund("Old Testament Abstracts")
get_databases("Seminary")

