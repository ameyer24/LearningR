###############################################################################
# Overview_____________________________________________________________________
###############################################################################

# This script looks at electronic resources usage and pricing information.
# It ingests DB1 and JR1 Counter Reports (Version 4)
# It allows libraries to import pricing information.
# It then calculates cost per use and other summaries.

###############################################################################
# Installing and Loading Packages _____________________________________________
###############################################################################
install.packages("tidyverse")
install.packages("readxl")
install.packages("xlsx")
install.packages("zoo")
install.packages("mosaic")
install.packages("scales")
library(mosaic)
library(xlsx)
library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)
library(scales)

###############################################################################
# Reading the files and tidying the data ______________________________________
###############################################################################

# Setting up the folders
input.folder <- "C:/DataScience/inputs"
output.folder <- "C:/DataScience/outputs"
DB1folder <- "C:/DataScience/inputs/DB1Reports"
JR1folder <- "C:/DataScience/inputs/JR1Reports"

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

# Get an overview of our data to make sure things imported relatively well.
DB1.summary <- DB1 %>%
  group_by(Publisher, Date) %>%
  summarise(Total_Usage = sum(Usage)) %>%
  spread(Date,Total_Usage) %>%
  write_csv(paste(output.folder, "DB1.usage.summary.csv",sep="/"))