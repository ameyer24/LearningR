###############################################################################
# Cost Per Use - Set Up________________________________________________________
###############################################################################

## Rework this as a function?!

# Need to combine usage data and cost data.
# summarized usage on the fiscal year.
cost.per.use.usage <- DB1 %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month > 6, Year + 1,Year)) %>%
  group_by(Database, Publisher,Platform, User_Activity, Fiscal_Year) %>%
  summarize(Usage = sum(Usage)) %>%
  na.omit()

# Transform the database pricing dataframe
cost.per.use.cost <- select(DB1.fin, -c(4:7))

# Merging everything together.
# Including everything from the usage dataframe.
cost.per.use <- merge(cost.per.use.usage, cost.per.use.cost, all.cost.per.use.usage = TRUE)

# Adds new column that calculates cost per user action.
cost.per.use$Cost_Per_Action <- cost.per.use$Cost/cost.per.use$Usage

# Replace all non-finite values with 0
# This solves the NaN and Inf problems introduced in the last step.
cost.per.use$Cost_Per_Action[!is.finite(cost.per.use$Cost_Per_Action)] <- 0

# Removes old data to keep everything nice and neat
rm(cost.per.use.usage, cost.per.use.cost)

###############################################################################
# Cost Per Use - Overview Functions ___________________________________________
###############################################################################

cpu.overview.1 <- function(df){
  df %>%
    select(-c(2:3,6:7)) %>%
    mutate(Cost_Per_Action = dollar(Cost_Per_Action)) %>%
    spread(Fiscal_Year, Cost_Per_Action)
}

# test <- cpu.overview.1(cost.per.use)

###############################################################################
# Cost Per Use - Detailed Functions ___________________________________________
###############################################################################

# Calculates the cost per use for a specific database over a range of time.
# Returns a table.
cpu.database <- function(DatabaseName,
                         StartYear,
                         EndYear,
                         Action = all.actions){
  cost.per.use %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(Database %in% DatabaseName) %>%
    filter(User_Activity %in% Action) %>%
    select(-c(2:3,6:7)) %>%
    mutate(Cost_Per_Action = dollar(Cost_Per_Action)) %>%
    spread(Fiscal_Year, Cost_Per_Action) %>%
    rename("User Activity" = "User_Activity")
}

test <- cpu.database(c("Communication & Mass Media Complete","Literary Reference Center"), 2014, 2018)
test <- cpu.database("Communication & Mass Media Complete", 2014, 2018, "Record Views")


# Set that as the default in case no action is specified.
cpu.database.graph <- function(DatabaseName,
                                StartYear,
                                EndYear,
                                Action = all.actions){
  cost.per.use %>%
    filter(Database %in% DatabaseName) %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    ggplot(aes(x = Fiscal_Year, y = Cost_Per_Action, group=1)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(labels=dollar) +
    facet_grid(User_Activity ~ ., scales = "free") +
    ggtitle(paste(DatabaseName)) +
    xlab("Fiscal Year") +
    ylab("Cost per Action")
}

cpu.database.graph("Communication & Mass Media Complete", 2010, 2018, "Record Views")
# cpu.database.graph("Communication & Mass Media Complete", 2014, 2018)
