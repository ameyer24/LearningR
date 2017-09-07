# A place to test out new things before putting them elsewhere.



# calculate the average price increase for databases per fund.
# Cost of databases grouped by fund and fiscal year.
# Accepts an option parameter to specify fund; defaults to all funds.
get.percent.changes <- function(StartYear, EndYear, SelectFund = all.funds){
  DB1.fin %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(Fund %in% SelectFund) %>%
    group_by(Database) %>%
    mutate(Price_Change = subfun.percent.cost.diff(Cost, lag(Cost))) %>%
    #mutate(Total_Cost = dollar(Total_Cost)) %>%
    select(-Cost) %>%
    group_by(Fund) %>%
    spread(Fiscal_Year, Price_Change) %>%
    write_csv(paste(output.folder, "cost.3.csv",sep="/"))
}
test = get.percent.changes (2014, 2018, "General")
