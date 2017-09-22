###############################################################################
# Installing Packages _________________________________________________________
###############################################################################

install.packages("tidyverse")
install.packages("stringr")

###############################################################################
# Loading Packages ____________________________________________________________
###############################################################################

library(tidyverse)
library(stringr)

###############################################################################
# Reading the files and tidying the data ______________________________________
###############################################################################

# Define path to folder that contains the OPAC logs.
OPAC_folder <- "Q:/OPAC_Logs"

# Define the Column Classes.
# NULL columns classes are skipped, we only want data in four fields.
vufind.col.classes <- c("character",
                           "NULL",
                           "NULL",
                           "NULL",
                           "character",
                           "NULL",
                           "character",
                           "NULL",
                           "NULL",
                           "NULL",
                           "character")

# Define the column names.
vufind.col.names <- c("AccessMethod",
                         "skipped",
                         "skipped",
                         "skipped",
                         "DateTime",
                         "skipped",
                         "SearchURL",
                         "skipped",
                         "skipped",
                         "skipped",
                         "Browser")

###############################################################################
# Reading files into R-------------------------------------------------
###############################################################################

# This function reads data from a single log file.
read.vufind.log <- function(file) {
  file %>%
    read.table(colClasses = vufind.col.classes,
               col.names = vufind.col.names,
               comment.char = "") %>%
    mutate(DateTime = as.POSIXct(DateTime,
                                 tz="GMT",
                                 format="[%d/%b/%Y:%H:%M:%S")) %>%
    separate(col=SearchURL, into = c("Method","SearchURL", "Protocol"), sep = " ")
}

#test1 <- read.vufind.log("Q:/OPAC_Logs/vufind_access_log.20170501.gz")

# This function reads data from a folder full of log files.
read.vufind.logs <- function(folder) {
  list.files(folder, full.names = T) %>%
  lapply(read.vufind.log) %>%
  bind_rows()
}

raw.vufind.data <- read.vufind.logs(OPAC_folder)

# Filters out non-NPU rows; returns only rows with 'vf-npu' in the SearchURL.
NPU.vufind.data <- filter(raw.vufind.data, grepl('vf-npu',SearchURL))

# Filters out only the lines that contain search information.
NPU.vufind.searches <- filter(NPU.vufind.data,grepl("Search/Home",SearchURL))

# Filters out only the lines that contain search information.
NPU.vufind.records <- filter(NPU.vufind.data, grepl("vf-npu/Record",SearchURL))


library(webreadr)
requests <- split_clf(NPU.vufind.data$SearchURL)


request2 <- separate(data=NPU.vufind.data, col=SearchURL, into = c("Method","SearchURL", "Protocol"), sep = " ")















###############################################################################
# Dividing the SearchURL-------------------------------------------------
###############################################################################

# Using regular expressions to parse out the SearchURL field.
# This regex returns the entire string divided into parts.
# Currently returns only the first five filters.

Vufind.SearchURL.regex <- "(GET) (.+[\\?])(lookfor.*?[\\&| ])?(type.*?[\\&| ])?(start.*?[\\&| ])?(submit.*?[\\&| ])?(only.*?[\\&| ])?(search.*?[\\&| ])?(filter.*?[\\&| ])?(filter.*?[\\&| ])?(filter.*?[\\&| ])?(filter.*?[\\&| ])?(filter.*?[\\&| ])?(view.*?[\\&| ])?(sort.*?[\\&| ])?(page.*?[\\&| ])?"

# Work on improving regex to return only the important parts.

Vufind.SearchURL.column.names <- c("GET",
                                   "FirstPart",
                                   "LookFor",
                                   "Type",
                                   "Start",
                                   "Submit",
                                   "OnlyMine",
                                   "Search",
                                   "Filter1",
                                   "Filter2",
                                   "Filter3",
                                   "Filter4",
                                   "Filter5",
                                   "View",
                                   "Sort",
                                   "Page")



###############################################################################
# Divides the SearchURL into useful parts--------------------------------------
###############################################################################

# This creates a new dataframe with the SearchURL divided into parts.
NPU.vufind.searches.divided <- extract(data = NPU.vufind.searches,
                                       col = SearchURL,
                                       into = Vufind.SearchURL.column.names,
                                       regex = Vufind.SearchURL.regex)

write.csv(NPU.vufind.searches.divided, "Q:/OPAC_Logs/NPUSearches.csv")

# A clunky step to clean up the data in the divided table.
# This can be extended to cover all cases.
# Perhaps better way to do this.
NPU.vufind.searches.clean <- NPU.vufind.searches.divided %>%
  mutate(LookFor = gsub("lookfor=","",LookFor)) %>%
  mutate(Type = gsub("type=","",Type))
  
write.csv(NPU.vufind.searches.clean, "C:/NPUSearches.csv")


