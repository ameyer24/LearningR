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
# Import Journal Usage Information____________________________________________
###############################################################################
# Defining Functions to Load JR1 Data.
load.JR1.csv <- function(path) { 
  csv.files <- dir(path, pattern = "*.(CSV|csv)", full.names = TRUE)
  tables <- lapply(csv.files, function(file){
    file %>%
      read_csv(skip=7, col_names = TRUE) %>%
      slice(-1) %>%
      subset(select = -c(8,9,10)) %>%
      gather(Date, Usage, -c(1:7)) %>%
      mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
      mutate(Usage = as.numeric(Usage))
  })
  do.call(rbind, tables)
}

load.JR1.excel <- function(path) { 
  excel.files <- dir(path, pattern = "*.xl*", full.names = TRUE)
  tables <- lapply(excel.files, function(file){
    file %>%
      read_excel(skip=7, col_names = TRUE) %>%
      slice(-1) %>%
      subset(select = -c(8,9,10)) %>%
      gather(Date, Usage, -c(1:7)) %>%
      mutate(Date = as.yearmon(Date, "%b-%Y")) %>%
      mutate(Usage = as.numeric(Usage))
  })
  do.call(rbind, tables)
}

# Creates dataframe with journal usage in a tidy format.
JR1 <-unique(rbind(load.JR1.csv(JR1folder),load.JR1.excel(JR1folder)))

JR1.summary <- JR1 %>%
  group_by(Platform, Date) %>%
  summarise(Total_Usage = sum(Usage)) %>%
  spread(Date,Total_Usage)