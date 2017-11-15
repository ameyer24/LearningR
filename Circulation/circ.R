###############################################################################
# Set Up _____________________________________________________________
###############################################################################
# I've decided to go with a data.table approach here.
# Load Packages
install.packages("data.table")
library(tidyverse)
library(lubridate)
library(data.table)

# Set up Files
circ.transactions <- fread(C:/DataScience/inputs/Circulation/circtransactions.csv)

circ_raw <- "C:/DataScience/inputs/Circulation/AJM - Circulation for R.txt"
# Set up the Column Names for the Import File
col_names = c("Circ_ID","Item_ID","Item_Type","Patron","Charge_Date","Due_Date")


###############################################################################
# Import and Tidy _____________________________________________________________
###############################################################################

# Importing the data.
circ_data <- read.csv(file = circ_raw, col.names = col_names)

# Cleaning up the date/time fields.
circ_data$Charge_Date <- parse_date_time(circ_data$Charge_Date,
                                         orders = "m/d/y H:M:S")
circ_data$Due_Date <- parse_date_time(circ_data$Due_Date,
                                         orders = "m/d/y H:M:S")

###############################################################################
# Transformations and Visualizations __________________________________________
###############################################################################

# Bar Plot of Circulation Transactions by Year
ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar()

# Bar Plot of Circulation Transactions by Year, Faceted by Patron Group
ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(Patron ~ .)

# Bar Plot of Circulation Transactions by Year, Faceted by Item Type
ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(Item_Type ~ .)

###############################################################################
# Transformations Collapsing Factors __________________________________________
###############################################################################

# Too many patron groups; I want to collapse them.
# Simple Example
circ_data$Patron

fct_collapse(circ_data$Patron,
             Student=c("Undergraduate","Graduate"),
             Staff=c("")
)

