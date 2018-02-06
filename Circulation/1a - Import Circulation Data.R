###############################################################################
# Set Up ______________________________________________________________________
###############################################################################
# Load Packages
library(tidyverse)
library(lubridate)

###############################################################################
# Import and Tidy _____________________________________________________________
###############################################################################

# Define path to data file
circ_file <- "C:/DataScience/inputs/Circulation/circtransactions.csv"

# Define column names for the circulation data in the file
circ_col_names = c("Circ_ID","Item_ID","Item_Type","Patron","Charge_Date","Due_Date")

# Import the data.
circ_data <- read.csv(circ_file, col.names = circ_col_names)

# Cleaning up the date/time fields.
circ_data$Charge_Date <- parse_date_time(circ_data$Charge_Date,
                                        orders = "m/d/y H:M:S")
circ_data$Due_Date <- parse_date_time(circ_data$Due_Date,
                                     orders = "m/d/y H:M:S")

# Remove variables.
rm(circ_file,circ_col_names)

