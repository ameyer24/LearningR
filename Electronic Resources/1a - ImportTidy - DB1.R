###############################################################################
# Import Database Usage Information____________________________________________
###############################################################################
# Transforms data from the standard Counter Format to a tidy dataframe.
tidy_reports <- function(df) {
  df %>%
    select(-c(5)) %>%
    gather(Date, Usage, -c(1:4)) %>%
    mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
    mutate(Usage = as.numeric(Usage)) %>%
    rename("User_Activity" = "User Activity")
}
# This function imports data from CSV files and makes that data tidy.
import_csv <- function(file) {
  file %>%
    read_csv(skip=7, col_names = TRUE) %>%
    tidy_reports()
}
# This function imports data from Excel files and makes that data tidy.
import_excel <- function(file) {
  file %>%
    read_excel(skip=7, col_names = TRUE) %>%
    tidy_reports()
}

# These functions load DB1 reports from a given folder.
load_csv <- function(path) { 
  csv_files <- dir(path, pattern = "*.(CSV|csv)", full.names = TRUE)
  tables <- lapply(csv_files, import_csv)
  do.call(rbind, tables)
}

load_excel <- function(path) { 
  excel_files <- dir(path, pattern = "*.(XL*|xl*)", full.names = TRUE)
  tables <- lapply(excel_files, import_excel)
  do.call(rbind, tables)
}

DB1_data_csv   <- load_csv(DB1_folder)
DB1_data_excel <- load_excel(DB1_folder)

DB1 <- unique(rbind(DB1_data_csv,DB1_data_excel))

# Create a variable with all user actions
# Need this for later functions to function
all_actions <- unique(DB1$User_Activity)
all_databases <- unique(DB1$Database)

###############################################################################
# Remove Unneeded Information _________________________________________________
###############################################################################
# Remove partial dataframes
rm(DB1_data_csv,DB1_data_excel)
# Remove import functions
rm(tidy_reports,import_csv,import_excel,load_csv,load_excel)


