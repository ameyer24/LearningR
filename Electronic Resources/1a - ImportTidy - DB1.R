###############################################################################
# Import Database Usage Information____________________________________________
###############################################################################

# Defining functions to load the data from DB1 Reports.
load.DB1.csv <- function(path) { 
  csv.files <- dir(path, pattern = "*.(CSV|csv)", full.names = TRUE)
  tables <- lapply(csv.files, function(file){
    file %>%
      read_csv(skip=7, col_names = TRUE) %>%
      subset(select = -c(5)) %>%
      gather(Date, Usage, -c(1:4)) %>%
      mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
      mutate(Usage = as.numeric(Usage)) %>%
      plyr::rename(replace = c("User Activity" = "User_Activity"))
  })
  do.call(rbind, tables)
}

load.DB1.excel <- function(path) { 
  excel.files <- dir(path, pattern = "*.xl*", full.names = TRUE)
  tables <- lapply(excel.files, function(file){
    file %>%
      read_excel(skip=7, col_names = TRUE) %>%
      subset(select = -c(5)) %>%
      gather(Date, Usage, -c(1:4)) %>%
      mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
      mutate(Usage = as.numeric(Usage)) %>%
      plyr::rename(replace = c("User Activity" = "User_Activity"))
  })
  do.call(rbind, tables)
}

# Creates dataframe with Database usage in a tidy format.
DB1 <-unique(rbind(load.DB1.csv(DB1folder),load.DB1.excel(DB1folder)))

# Create a variable with all user actions
# Need this for later functions to function
all.actions = unique(DB1$User_Activity)

## Add some more data quality control steps here?

# # Get an overview of our data to make sure things imported relatively well.
# DB1.summary <- DB1 %>%
#   group_by(Publisher, Date) %>%
#   summarise(Total_Usage = sum(Usage)) %>%
#   spread(Date,Total_Usage) %>%
#   write_csv(paste(output.folder, "DB1.usage.summary.csv",sep="/"))


