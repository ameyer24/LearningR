###############################################################################
# Usage Overivew Functions ____________________________________________________
###############################################################################
# Returns table of usage for a particular database between two years.
select_database_usage <- function(DatabaseName,
                                  StartYear,
                                  EndYear,
                                  Action = all_actions){
  DB1 %>%
    filter(Database %in% DatabaseName) %>%
    filter(Date >= StartYear, Date <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    spread(Date,Usage) %>%
    write_csv(paste(output_folder, "usage.table.1.csv",sep="/")) %>%
    return()
}
test <- select_database_usage("Database A", 2014, 2018)

graph_database_usage <- function(DatabaseName,
                                 StartYear,
                                 EndYear,
                                 Action = all_actions){
  DB1 %>%
    filter(Database %in% DatabaseName) %>%
    filter(Date >= StartYear, Date <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    ggplot(aes(Date, Usage)) +
    geom_line() +
    scale_x_yearmon() +
    facet_grid(User_Activity ~ ., scales = "free")
}
graph_database_usage("Database A", 2014, 2018)

# Summarize database usage by academic term.
summarize_usage_academic_term <- function(DatabaseName,
                                         StartYear,
                                         EndYear,
                                         Action = all_actions){
  DB1 %>%
    filter(Database %in% DatabaseName) %>%
    filter(Date >= StartYear, Date <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    mutate(Year = year(Date), Month=month(Date)) %>%
    mutate(Academic_Term = derivedFactor(
      "Spring" = (Month==1 | Month==2  | Month==3  | Month==4),
      "Summer" = (Month==5 | Month==6  | Month==7  | Month==8),
      "Fall"   = (Month==9 | Month==10 | Month==11 | Month==12)
    )) %>%
    group_by(Database,User_Activity,Academic_Term,Year) %>%
    summarize(Usage=sum(Usage)) %>%
    rename("User Activity" = "User_Activity")
}

summarized_usage_academic_term <- summarize_usage_academic_term("Database A", 2014, 2018)

# Creates a bar plot of the usage data summarized on the academic term.
ggplot(data = summarized_usage_academic_term,
       aes(x = Year, y = Usage)) +
  geom_bar(aes(fill=factor(Year)),stat="identity") +
  facet_grid(. ~ Academic_Term) +
  labs(y = "Usage", fill="Year") +
  ggtitle("Database Usage by Academic Term") +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

# This function goes directly from the dataframe to the barplot.
graph_usage_academic_term <- function(DatabaseName,
                                           StartYear,
                                           EndYear,
                                           Action = all_actions){
  DB1 %>%
    # filters based on database name, date, and user activity
    filter(Database %in% DatabaseName) %>%
    filter(Date >= StartYear, Date <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    # creates new variables "Year" and "Month" from the date field.
    mutate(Year = year(Date), Month=month(Date)) %>%
    # creates new variable "Academic_Term" based on the month data.
    mutate(Academic_Term = derivedFactor(
      "Spring" = (Month==1 | Month==2  | Month==3  | Month==4),
      "Summer" = (Month==5 | Month==6  | Month==7  | Month==8),
      "Fall"   = (Month==9 | Month==10 | Month==11 | Month==12)
    )) %>%
    group_by(Database, User_Activity, Academic_Term, Year) %>%
    summarize(Usage=sum(Usage)) %>%
    ggplot(aes(x = Year, y = Usage)) +
    geom_bar(aes(fill=factor(Year)),
             stat="identity") +
    facet_grid(. ~ Academic_Term) +
    xlab("Year and Academic Term") +
    ylab("Usage")
}
graph_usage_academic_term("Database A", 2014, 2018, "Record Views")

