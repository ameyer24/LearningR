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
# It's a mess but it's a start!
# Currently returns only the first five filters.

Vufind.SearchURL.pattern <- "(GET) (.+[\\?])(lookfor.*?[\\&| ])?(type.*?[\\&| ])?(start.*?[\\&| ])?(submit.*?[\\&| ])?(only.*?[\\&| ])?(search.*?[\\&| ])?(filter.*?[\\&| ])?(filter.*?[\\&| ])?(filter.*?[\\&| ])?(filter.*?[\\&| ])?(filter.*?[\\&| ])?(view.*?[\\&| ])?(sort.*?[\\&| ])?(page.*?[\\&| ])?"

Vufind.SearchURL.column.names <- c("cat1","c2","c3","c4","c5","c6","c7","c8","c9","c11","c11","c12","c13","c14","c15","c16","c17")

###############################################################################
# Reading the log files into R-------------------------------------------------
###############################################################################

# Testing Things Out one file at a time.
raw.vufind.data <- read.table("Q:/OPAC_Logs/vufind_access_log.20170304.gz",
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

NPU.vufind.data <- raw.vufind.data %>%
  filter(grepl('vf-npu',SearchURL)) %>%
  mutate(DateTime = as.POSIXct(DateTime,
                               tz="GMT",
                               format="[%d/%b/%Y:%H:%M:%S"))



NPU.vufind.searches <- NPU.vufind.data %>%
  filter(grepl("Search/Home",SearchURL))


# Divides the SearchURL field into parts.
test1 <- str_match(NPU.vufind.searches$SearchURL, Vufind.SearchURL.pattern)
test2 <- str_detect(NPU.vufind.searches$SearchURL, Vufind.SearchURL.pattern)
test3 <- separate(data = NPU.vufind.searches,
           col = SearchURL,
           into = Vufind.SearchURL.column.names,
           sep = Vufind.SearchURL.pattern)

