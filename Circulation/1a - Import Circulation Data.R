###############################################################################
# Set Up ______________________________________________________________________
###############################################################################
# Load Packages
library(tidyverse)
library(lubridate)
library(data.table)

# Define path to data file
circ.file <- "C:/DataScience/inputs/Circulation/circtransactions.csv"
# Define column names for the circulation data in the file
col.names = c("CircID","ItemID","ItemType","Patron","ChargeDate","DueDate")


###############################################################################
# Import and Tidy _____________________________________________________________
###############################################################################
# Import the data.
circ.data <- read.csv(circ.file, col.names = col.names)

# Cleaning up the date/time fields.
circ.data$ChargeDate <- parse_date_time(circ.data$ChargeDate,
                                        orders = "m/d/y H:M:S")
circ.data$DueDate <- parse_date_time(circ.data$DueDate,
                                     orders = "m/d/y H:M:S")

# Remove variables.
rm(circ.file,col.names)
