###############################################################################
# Import and Tidy Item Information ____________________________________________
###############################################################################
# Path to the item informaiton file
item.file <- "C:/DataScience/inputs/Circulation/itemdata.csv"
# Column names for item data
item.col.names = c("ItemID","Call#","LCClass","Title")
# This is the item information
item.data <- read.csv(item.file, col.names = item.col.names)

rm(item.file, item.col.names)


