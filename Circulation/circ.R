###############################################################################
# Reading the files and tidying the data ______________________________________
###############################################################################

# Setting up the folders and file pathways 
input <- "C:/DataScience/inputs"
output <- "C:/DataScience/outputs"
circ_raw <- "C:/DataScience/inputs/Circulation/Circulation.txt"

# Setting up the file headers
circ_col_names = c("Circ_ID","Item_ID","Date_Time","Patron","Item_Type","Acad_Year","Acad_Term","Week_of_sem")

# Importing the data.
circ_data <- read.csv(file = circ_raw, col.names = circ_col_names)
circ_data <- circ_data[-c(6:8)] # Deleting my "special" addition for this test.

