###############################################################################
# Import and Tidy Item Information ____________________________________________
###############################################################################
item.file <- "C:/DataScience/inputs/Circulation/itemdata.csv"
item.col.names = c("ItemID","Call#","LCClass","Title")
# This is the item information
item.data <- read.csv(item.file, col.names = item.col.names)

# This merges the two dataframes to create a new one.
# Keeps all circulation transactions and add items info when available.
test <- merge(x = circ.data, y= item.data, by = "ItemID", all.x = TRUE)
