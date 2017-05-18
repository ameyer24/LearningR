install.packages("tidyverse")
library(tidyverse)

## Setting things up
## Define path to folder that contains the OPAC logs.
folder <- "Q:/OPAC_Logs"

## Defines the Column Classes. The "NULL" columns don't contain useful information and will be skipped.
VuFind_Col_Classes <- c("character","NULL","NULL","NULL","character","NULL","character","NULL","NULL","NULL","character")
## Defines the column names.
VuFind_Col_Names <- c("AccessMethod","test","test","test","DateTime","test","SearchURL","test","test","test","Browser")

# ## Testing Things Out one file at a time.
# VuFind_Data <- read.table("Q:/OPAC_Logs/vufind_access_log.20170304.gz",
#            colClasses = VuFind_Col_Classes,
#            col.names = VuFind_Col_Names)
# 
# NPU_VuFind <- VuFind_Data %>%
#   filter(grepl('vf-npu',SearchURL)) %>%
#   mutate(DateTime = as.POSIXct(DateTime, tz="GMT", format="[%d/%b/%Y:%H:%M:%S")) %>%
#   filter(grepl('lookfor',SearchURL))
  
## Can I read a bunch of OPAC logs? Yes! Is this smart?
## Also good news - no need to unzip the files!
log_files <- list.files(folder, full.names = T)
VuFind_Data <- do.call(rbind, lapply(log_files,
                                      read.table, 
                                      colClasses = VuFind_Col_Classes,
                                      col.names = VuFind_Col_Names)
                        )

NPU_VuFind <- VuFind_Data %>%
  filter(grepl('vf-npu',SearchURL)) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz="GMT", format="[%d/%b/%Y:%H:%M:%S")) %>%
  filter(grepl('lookfor',SearchURL))
