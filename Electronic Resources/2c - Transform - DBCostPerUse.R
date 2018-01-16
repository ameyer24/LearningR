###############################################################################
# Cost Per Use - Set Up________________________________________________________
###############################################################################
# Need to combine monthly usage data with yearly financial data
# Step 1. Summarize usage data by fiscal year.
CPU_usage <- DB1 %>%
  mutate(Year = year(Date), Month=month(Date)) %>%
  mutate(Fiscal_Year = ifelse(Month > 6, Year + 1,Year)) %>%
  group_by(Database, Publisher,Platform, User_Activity, Fiscal_Year) %>%
  summarize(Usage = sum(Usage)) %>%
  na.omit()

# Step 2: Transform the financial information to match
CPU_cost <- select(DB1_financial, -c(4:7))

# Step 3: Merge usage information and financial information
# Including everything from the usage dataframe.
CPU <- merge(CPU_usage, CPU_cost, all.CPU_usage = TRUE)

# Step 4: Transform: Calculate Cost Per Action
CPU$Cost_Per_Action <- CPU$Cost/CPU$Usage

# Step 5: Clean Up: Replace all non-finite values with 0
# This solves the NaN and Inf problems introduced in the last step.
CPU$Cost_Per_Action[!is.finite(CPU$Cost_Per_Action)] <- 0

# Removes old data to keep everything nice and neat
rm(CPU_usage, CPU_cost)

###############################################################################
# Cost Per Use - Overview Functions ___________________________________________
###############################################################################

cpu.overview.1 <- function(StartYear,
                           EndYear,
                           Action = all_actions){
  CPU %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    select(-Usage,-Cost) %>%
    mutate(Cost_Per_Action = dollar(Cost_Per_Action)) %>%
    spread(Fiscal_Year, Cost_Per_Action)
}

test <- cpu.overview.1(2014, 2018, "Regular Searches")

# Calculates the average cost per action - grouped by year.
# updated to include the median cost as well.
cpu.overview.2 <- function(StartYear,
                           EndYear,
                           Action = all_actions){
  CPU %>%
    filter(Fiscal_Year >= StartYear, Fiscal_Year <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    select(-Usage,-Cost) %>%
    group_by(User_Activity, Fiscal_Year) %>%
    summarize(Median.CPA = median(Cost_Per_Action),
              Average.CPA = mean(Cost_Per_Action)) %>%
    mutate(Median.CPA = dollar(Median.CPA),
           Average.CPA = dollar(Average.CPA))
}

test <- cpu.overview.2(2014, 2018,"Record Views")

###############################################################################
# Cost Per Use - Detailed Functions ___________________________________________
###############################################################################

# Calculates the cost per use for a specific database over a range of time.
# Returns a table.
cpu.database <- function(DatabaseName,
                         StartYear,
                         EndYear,
                         Action = all_actions){
  CPU %>%
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
                                Action = all_actions){
  CPU %>%
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