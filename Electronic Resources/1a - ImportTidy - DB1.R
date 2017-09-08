###############################################################################
# Import Database Usage Information____________________________________________
###############################################################################

# Transforms data from the standard Counter Format to a tidy dataframe.
tidy.DB1reports <- function(df) {
  df %>%
    select(-c(5)) %>%
    gather(Date, Usage, -c(1:4)) %>%
    mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
    mutate(Usage = as.numeric(Usage)) %>%
    rename("User_Activity" = "User Activity")
}
# This function imports data from CSV files and makes that data tidy.
csv.importer <- function(file) {
  file %>%
    read_csv(skip=7, col_names = TRUE) %>%
    tidy.DB1reports()
}
# This function imports data from Excel files and makes that data tidy.
excel.importer <- function(file) {
  file %>%
    read_excel(skip=7, col_names = TRUE) %>%
    tidy.DB1reports()
}

# These functions load DB1 reports from a given folder.
load.DB1.csv <- function(path) { 
  csv.files <- dir(path, pattern = "*.(CSV|csv)", full.names = TRUE)
  tables <- lapply(csv.files, csv.importer)
  do.call(rbind, tables)
}

load.DB1.excel <- function(path) { 
  excel.files <- dir(path, pattern = "*.(XL*|xl*)", full.names = TRUE)
  tables <- lapply(excel.files, excel.importer)
  do.call(rbind, tables)
}

DB1.data.csv <- load.DB1.csv(DB1folder)
DB1.data.excel <- load.DB1.excel(DB1folder)

DB1 <- unique(rbind(DB1.data.csv,DB1.data.excel))


# Create a variable with all user actions
# Need this for later functions to function
all.actions <- unique(DB1$User_Activity)

# Removes temporary dataframes to keep things neat
rm(DB1.data.csv,DB1.data.excel)
