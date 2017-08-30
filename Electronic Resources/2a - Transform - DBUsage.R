###############################################################################
# Usage Overivew Functions ____________________________________________________
###############################################################################

# Returns table of usage for particular database between two years.
usage.table.1 <- function(DatabaseName,StartYear,EndYear){
  DB1 %>%
    filter(Database==DatabaseName) %>%
    filter(Date >= StartYear, Date <= EndYear) %>%
    spread(Date,Usage) %>%
    write_csv(paste(output.folder, "usage.table.1.csv",sep="/"))
}
test1 <- usage.table.1("New Testament Abstracts", 2015, 2018)
test2 <- usage.table.1("Old Testament Abstracts", 2015, 2018)

# Graphing database usage.
usage.graph.1 <- function(DatabaseName,StartYear,EndYear){
  DB1 %>%
    filter(Database==DatabaseName) %>%
    filter(Date > StartYear, Date < EndYear) %>%
    ggplot(aes(Date, Usage)) +
    geom_line() +
    geom_smooth(span=0.7) +
    scale_x_yearmon() +
    facet_grid(User_Activity ~ ., scales = "free")
}
usage.graph.1("Academic Search Complete", 2015, 2018)

# Graphing database usage without automated searching.
usage.graph.2 <- function(DatabaseName,StartYear,EndYear){
  DB1 %>%
    filter(Database == DatabaseName) %>%
    filter(Date > StartYear, Date < EndYear) %>%
    filter(User_Activity != "Searches-federated and automated") %>%
    ggplot(aes(Date, Usage)) +
    geom_line() +
    geom_smooth(span=0.7) +
    scale_x_yearmon() +
    facet_grid(User_Activity ~ .)
}
usage.graph.2("Business Source Complete", 2015, 2018)

# Graphing database usage by academic term.
usage.graph.3 <- function(DatabaseName,StartYear,EndYear){
  DB1 %>%
    filter(Database == DatabaseName) %>%
    filter(Date > StartYear, Date < EndYear) %>%
    filter(User_Activity != "Searches-federated and automated") %>%
    mutate(Year = year(Date), Month=month(Date)) %>%
    filter(Year>2014) %>%
    mutate(Academic_Term = derivedFactor(
      "S1 (Spring)" = (Month==1 | Month==2 | Month==3 | Month==4),
      "S2 (Summer)" = (Month==5 | Month==6 | Month==7 | Month==8),
      "S3 (Fall)" = (Month==9 | Month==10 | Month==11 | Month==12),
      .default = "Unknown"
    )) %>%
    mutate(Academic_Year = paste(Year, Academic_Term, sep=" "))%>%
    group_by(Database, Publisher, Platform, User_Activity, Academic_Year) %>%
    summarize(Usage=sum(Usage)) %>%
    # Graphs the data on a line graph.
    ggplot(aes(Academic_Year,Usage)) +
    facet_grid(User_Activity ~ .) + 
    geom_line(aes(group=User_Activity))
}
usage.graph.3("Business Source Complete", 2015, 2018)