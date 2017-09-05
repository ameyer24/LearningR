###############################################################################
# Usage Overivew Functions ____________________________________________________
###############################################################################

# Returns table of usage for a particular database between two years.
usage.select.database <- function(DatabaseName,
                                  StartYear,
                                  EndYear,
                                  Action = all.actions){
  DB1 %>%
    filter(Database==DatabaseName) %>%
    filter(Date >= StartYear, Date <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    spread(Date,Usage) %>%
    write_csv(paste(output.folder, "usage.table.1.csv",sep="/")) %>%
    return()
}

# Graphing database usage.
usage.graph.database <- function(DatabaseName,
                                 StartYear,
                                 EndYear,
                                 Action = all.actions){
  DB1 %>%
    filter(Database==DatabaseName) %>%
    filter(Date >= StartYear, Date <= EndYear) %>%
    filter(User_Activity %in% Action) %>%
    ggplot(aes(Date, Usage)) +
    geom_line() +
    geom_smooth(span=0.7) +
    scale_x_yearmon() +
    facet_grid(User_Activity ~ ., scales = "free")
}
usage.graph.database("Communication & Mass Media Complete", 2014, 2018)

# Sums database usage by academic term.
usage.sum.database.acad.term <- function(DatabaseName,
                                         StartYear,
                                         EndYear,
                                         Action = all.actions){
  DB1 %>%
    filter(Database == DatabaseName) %>%
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
    spread(Academic_Term, Usage)
}
test1 <- usage.sum.database.acad.term("Communication & Mass Media Complete", 2014, 2018)


# Graphs database usage based on academic term.
usage.graph.database.acad.term <- function(DatabaseName,
                                           StartYear,
                                           EndYear,
                                           Action = all.actions){
  DB1 %>%
    filter(Database == DatabaseName) %>%
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
    ggplot(aes(x = Academic_Term, y = Usage)) +
    geom_bar(aes(fill=factor(Year)),
             stat="identity",
             position = position_dodge())
}
usage.graph.database.acad.term("Communication & Mass Media Complete", 2014, 2018, "Record Views")

