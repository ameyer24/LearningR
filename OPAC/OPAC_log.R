install.packages("tidyverse")
install.packages("stringr")
library(tidyverse)
library(stringr)


# Define path to folder that contains the OPAC logs.
folder <- "Q:/OPAC_Logs"

# Define the Column Classes. NULL columns classes are skipped.
vufind.column.classes <- c("character",
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
vufind.column.names <- c("AccessMethod",
                         "test",
                         "test",
                         "test",
                         "DateTime",
                         "test",
                         "SearchURL",
                         "test",
                         "test",
                         "test",
                         "Browser")

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
# Reading the log files into R-------------------------------------------------
###############################################################################

# Testing Things Out one file at a time.
raw.vufind.data <- read.table("Q:/OPAC_Logs/vufind_access_log.20170501.gz",
           colClasses = vufind.column.classes,
           col.names = vufind.column.names,
           comment.char = "")

# Reads all the files from the given folder.
log.files <- list.files(folder, full.names = T)
raw.vufind.data <- do.call(rbind, lapply(log.files,
                                         read.table,
                                         colClasses = vufind.column.classes,
                                         col.names = vufind.column.names,
                                         comment.char = ""))

###############################################################################
# Filtering the data by institution--------------------------------------------
###############################################################################

# Filters out only NPU lines.
NPU.vufind.data <- raw.vufind.data %>%
  filter(grepl('vf-npu',SearchURL)) %>%
  mutate(DateTime = as.POSIXct(DateTime,
                               tz="GMT",
                               format="[%d/%b/%Y:%H:%M:%S"))
###############################################################################
# Filtering the data for only searches-----------------------------------------
###############################################################################

# Filters out only the lines that contain search information.
NPU.vufind.searches <- NPU.vufind.data %>%
  filter(grepl("Search/Home",SearchURL))

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

###############################################################################
# Filtering the data for only record views-------------------------------------
###############################################################################

# Filters out only the lines that contain search information.
NPU.vufind.records <- NPU.vufind.data %>%
  filter(grepl("vf-npu/Record",SearchURL))
