install.packages("tidyverse")
library(tidyverse)

# Setting things up
# Define path to folder that contains the OPAC logs.
folder <- "Q:/OPAC_Logs"

# Defines the Column Classes.
# The "NULL" columns don't contain useful information and will be skipped.
vufind.column.classes <- c("character","NULL","NULL","NULL","character","NULL","character","NULL","NULL","NULL","character")
# Defines the column names.
vufind.column.names <- c("AccessMethod","test","test","test","DateTime","test","SearchURL","test","test","test","Browser")

# Testing Things Out one file at a time.
raw.vufind.data <- read.table("Q:/OPAC_Logs/vufind_access_log.20170304.gz",
           colClasses = vufind.column.classes,
           col.names = vufind.column.names)

NPU.vufind <- raw.vufind.data %>%
  filter(grepl('vf-npu',SearchURL)) %>%
  mutate(DateTime = as.POSIXct(DateTime,
                               tz="GMT",
                               format="[%d/%b/%Y:%H:%M:%S")) %>%
  filter(grepl('lookfor',SearchURL))
  
## Can I read a bunch of OPAC logs? Yes! Is this smart?
log.files <- list.files(folder, full.names = T)
raw.vufind.data <- do.call(rbind, lapply(log.files,
                                      read.table, 
                                      colClasses = vufind.column.classes,
                                      col.names = vufind.column.names)
                        )

NPU.vufind <- raw.vufind.data %>%
  filter(grepl('vf-npu',SearchURL)) %>%
  mutate(DateTime = as.POSIXct(DateTime,
                               tz="GMT",
                               format="[%d/%b/%Y:%H:%M:%S")) %>%
  filter(grepl('lookfor',SearchURL))
