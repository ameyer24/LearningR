# A place to test out new things before putting them elsewhere.

###############################################################################
# Cost Increase Functions _____________________________________________________
###############################################################################
# I wanted a function to compute average price increases
# Still a work in progress but it's working decently well now.

get.percent.change <- function(){
  DB1_financial %>%
    select(-Publisher,-Platform,-Order_Agent,-Order_Term) %>%
    group_by(Database) %>%
    mutate(Percent_Increase = (Cost-lag(Cost))/lag(Cost)) %>%
    arrange(Database,Fiscal_Year) %>%
    subset(!is.na(Percent_Increase))
}

DB1.percent.change <- get.percent.change()

# I want to manually remove some values.
DB1.percent.change <- DB1.percent.change %>%
  filter(!(Database == "Project MUSE" & Fiscal_Year == 2017)) %>%
  filter(!(Database == "SAGE Premier ejournals (2012)" & Fiscal_Year == 2017)) %>%
  filter(!(Database == "JSTOR" & Fiscal_Year == 2015)) %>%
  filter(!(Database == "Foundation Directory Online")) %>%
  filter(!(Database == "STAT!Ref Online Resource"))

get.percent.change.fund <- function(StartYear, EndYear, SelectFund = all.funds){
  DB1.percent.change %>%
    filter(Fiscal_Year <= EndYear) %>%
    filter(Fiscal_Year >= StartYear) %>%
    filter(Fund %in% SelectFund) %>%
    ungroup() %>%
    group_by(Database, Fund, Fiscal_Year) %>%
    summarize(Average_Percent_Increase = percent(mean(Percent_Increase))) %>%
    spread(Fiscal_Year,Average_Percent_Increase) %>%
    arrange(Fund)
}
