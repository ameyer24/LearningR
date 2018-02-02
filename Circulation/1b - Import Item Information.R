###############################################################################
# Import and Tidy Item Information ____________________________________________
###############################################################################
# Path to the item informaiton file
item_file <- "C:/DataScience/inputs/Circulation/itemdata.csv"
# Column names for item data
item_col_names = c("Item_ID","Call_Number","LC_Class","Title","Item_Type")
# This is the item information
item_data <- read.csv(item_file, col.names = item_col_names)

rm(item_file, item_col_names)

# This creates a factor with the first letter from the LCClass
item_data$LC_Letter <- as.factor(substring(item_data$LC_Class,1,1))
levels(item_data$LC_Letter)
