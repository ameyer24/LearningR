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
