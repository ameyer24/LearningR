install.packages("xlsx")
install.packages("zoo")
library(readxl)
library(zoo)

# Path to the eBook Usage folder - contains only Excel files.
eBook_Usage_Folder <- "C:/DataScience/inputs/Circulation/eBook_Usage"

# Transforms data from the standard Counter Format to a tidy dataframe.
tidy.BR1reports <- function(df) {
  df %>%
    select(-c(7,8)) %>% # Deletes the ISSN and Reporting Period Total
    gather(Date, Usage, -c(1:6)) %>%
    mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
    mutate(Usage = as.numeric(Usage))
}

# This function imports data from Excel files and makes that data tidy.
excel.importer <- function(file) {
  file %>%
    read_excel(skip=7, col_names = TRUE) %>%
    tidy.BR1reports()
}

# This functions load DB1 reports from a given folder.
load.BR1.excel <- function(path) { 
  excel.files <- dir(path, pattern = "*.(XL*|xl*)", full.names = TRUE)
  tables <- lapply(excel.files, excel.importer)
  do.call(rbind, tables)
}

BR1.usage.data <- load.BR1.excel(eBook_Usage_Folder)

# Drop values with NA Properitary ID
# Do this at the merge step?
