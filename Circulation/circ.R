###############################################################################
# Reading the files and tidying the data ______________________________________
###############################################################################

library(lubridate)
# Setting up the file pathway
circ_raw <- "C:/DataScience/inputs/Circulation/AJM - Circulation for R.txt"

# Setting up the file headers
col_names = c("Circ_ID","Item_ID","Item_Type","Patron","Charge_Date","Due_Date")

# Importing the data.
circ_data <- read.csv(file = circ_raw,
                      col.names = circ_col_names)



circ_data$Charge_Date <- as.Date(circ_data$Charge_Date)


circ_data$Charge_Date <- strptime(x = as.character(circ_data$Charge_Date),
                                  format = "%d/%m/%Y %H:%M")

circ_data$Due_Date <- strptime(x = as.character(circ_data$Due_Date),
                                  format = "%d/%m/%Y %H:%M")


