###############################################################################
# Reading the files and tidying the data ______________________________________
###############################################################################

library(tidyverse)
# Setting up the file pathway
circ_raw <- "C:/DataScience/inputs/Circulation/AJM - Circulation for R.txt"

# Setting up the file headers
col_names = c("Circ_ID","Item_ID","Item_Type","Patron","Charge_Date","Due_Date")

# Importing the data.
circ_data <- read.csv(file = circ_raw, col.names = circ_col_names)

# Cleaning up the date/time fields.
circ_data$Charge_Date <- parse_date_time(circ_data$Charge_Date,
                                         orders = "m/d/y H:M:S")
circ_data$Due_Date <- parse_date_time(circ_data$Due_Date,
                                         orders = "m/d/y H:M:S")

###############################################################################
# Transformations and Visualizations __________________________________________
###############################################################################

ggplot(circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  theme_minimal()
